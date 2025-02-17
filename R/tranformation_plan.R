# transformation plan

tranformation_plan <- list(

  # climate data
  tar_target(
    name = daily_temp,
    command = climate_raw %>%
      mutate(date = dmy(format(date_time, "%d.%b.%Y"))) |>
      # daily temperature
      group_by(date, destSiteID, destBlockID, destPlotID, origSiteID, origBlockID, warming, Nlevel, grazing, Namount_kg_ha_y) %>%
      summarise(air_temperature = mean(air_temperature, na.rm = TRUE),
                ground_temperature = mean(ground_temperature, na.rm = TRUE),
                soil_temperature = mean(soil_temperature, na.rm = TRUE),
                soilmoisture = mean(soilmoisture, na.rm = TRUE)) |>
      pivot_longer(cols = c(air_temperature:soilmoisture), names_to = "variable", values_to = "value") |>
      filter(!is.na(value)) |>
      # prettify
      mutate(warming = recode(warming, A = "Ambient", W = "Warming"),
             grazing = recode(grazing, C = "Control", M = "Medium", I = "Intensive"),
             grazing = factor(grazing, levels = c("Control", "Medium", "Intensive")),
             origSiteID = recode(origSiteID, Joa = "Sub-alpine", Lia = "Alpine"),
             variable = recode(variable, air_temperature = "air", ground_temperature = "ground", soil_temperature = "soil"))

  ),

  ## gridded climate data
  # monthly
  tar_target(
    name = monthly_climate,
    command = gridded_climate_raw %>%
      filter(variable %in% c("temperature", "precipitation")) |>
      # monthly data
      mutate(year = year(date),
             date_month = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
      group_by(year, date_month, variable, siteID) %>%
      summarise(sum = sum(value),
                value = mean(value)) |>
      mutate(value = ifelse(variable == "precipitation", sum, value)) %>%
      select(-sum)

  ),

  # annual precipitation and summer temperature
  tar_target(
    name = annual_climate,
    command = monthly_climate %>%
      mutate(month = month(date_month)) |>
      filter(variable == "temperature" & month %in% 6:9 | variable == "precipitation") |>
      group_by(year, siteID, variable) %>%
      summarise(se = sd(value)/sqrt(n()),
                sum = sum(value),
                value = mean(value)) |>
      mutate(value = ifelse(variable == "precipitation", sum, value)) %>%
      select(-sum)

  ),


  # biomass
  # (scale to m2, 2022 and prettify dataset)
  tar_target(
    name = biomass,
    command = {
      biomass_raw %>%
        # calculate area in m2 and scale biomass to m2
        mutate(area_m2 = area_cm2 / 10000,
               biomass_scaled = biomass / area_m2
        ) %>%
        # only useful data for last year
        filter(year %in% c(2022)) %>%

        # remove 150 kg N
        filter(Namount_kg_ha_y != 150) %>%

        # log transform Nitrogen
        mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

        # prettify
        mutate(origSiteID = recode(origSiteID, "Liahovden" = "Alpine", "Joasete" = "Sub-alpine"),
               origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
               grazing = factor(grazing, levels = c("C", "M", "I", "N")),
               grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
               warming = recode(warming, "A" = "Ambient", "W" = "Warming")) |>


        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
               grazing_num = as.numeric(grazing_num))

    }),


  # Measured standing biomass
  # (calc remaining biomass from control - cuts)
  tar_target(
    name = measured_standing_biomass,
    command =  biomass |>
      # get peak biomass and remove litter
      filter(grazing == "Control" & cut == 3| grazing %in% c("Medium", "Intensive") & cut == 4) |>
      filter(!fun_group %in% c("litter")) |>
      group_by(origSiteID, destSiteID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing) |>
      summarise(biomass = sum(biomass), .groups = "drop") |>
      pivot_wider(names_from = grazing, values_from = biomass) |>
      # remaining biomass from control - cutting
      mutate(Medium = Control - Medium,
             Intensive = Control - Intensive) |>
      pivot_longer(cols = c(Control, Medium, Intensive), names_to = "grazing", values_to = "biomass_remaining_coll")
  ),

  # Estimate standing biomass
  # (from species cover and height)
  tar_target(
    name = estimated_standing_biomass,
    command = cover_total |>
      filter(year %in% c(2019, 2022)) |>
      group_by(year, origSiteID, warming, grazing, Nlevel, Namount_kg_ha_y, Nitrogen_log) |>
      summarise(sum_cover = sum(cover)) |>
      left_join(height |>
                  filter(vegetation_layer == "Vascular plant layer") |>
                  select(-destSiteID, -vegetation_layer, -delta) |>
                  pivot_longer(cols = c(`2019`, `2022`), names_to = "year", values_to = "height") |>
                  mutate(year = as.numeric(year)),
                         by = c("year", "origSiteID", "warming", "grazing", "Namount_kg_ha_y")) |>
      mutate(biomass_remaining_calc = sum_cover * height) |>
      select(-grazing_num)
      ),

  # ESTIMATE STANDING BIOMASS FROM COVER X HEIGHT
  # prep data
  tar_target(
    name = prep_SB_back,
    command = estimated_standing_biomass |>
        select(-sum_cover, -height) |>
        # join collected biomass from control plots
        tidylog::left_join(measured_standing_biomass |>
                             filter(grazing == "Control"))
  ),

  # run speparate models for first and last year
  # including a correction for nitrogen in 2022
  tar_target(
    name = SB_back_model_22,
    command = lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log, data = prep_SB_back |>
                   filter(grazing == "Control",
                          year == 2022))
  ),

  tar_target(
    name = SB_back_model_19,
    command = lm(biomass_remaining_coll ~ biomass_remaining_calc, data = prep_SB_back |>
                   filter(grazing == "Control",
                          year == 2019))
  ),

  # back transform calculated biomass using model from control plots
  tar_target(
    name = standing_biomass_back,
    command = {

    bind_rows(
      # 2022 data
      augment(x = SB_back_model_22, newdata = prep_SB_back |>
                filter(grazing != "Control",
                       year == 2022)) |>
        rename(standing_biomass = .fitted) |>
        select(-biomass_remaining_calc, -biomass_remaining_coll),

      # 2019 data
      augment(x = SB_back_model_19, newdata = prep_SB_back |>
                filter(year == 2019)) |>
        rename(standing_biomass = .fitted) |>
        select(-biomass_remaining_calc, -biomass_remaining_coll)
    ) |>
      # add collected standing biomass for control plots
      bind_rows(measured_standing_biomass |>
                  filter(grazing == "Control") |>
                  mutate(year = 2022) |>
                  rename(standing_biomass = biomass_remaining_coll)) |>
      select(-destSiteID, -.resid)

    }
  ),


  ### Needed?
  # Estimate consumption from standing biomass
  # (from species cover and height)
  tar_target(
    name = estimated_consumption,
    command = {

      estimated_standing_biomass |>
        ungroup() |>
        select(-sum_cover, -height) |>
        pivot_wider(names_from = grazing, values_from = biomass_remaining_calc) |>
        mutate(Medium = Control - Medium,
               Intensive = Control - Intensive,
               Natural = Control - Natural,
               Control = 0) |>
        pivot_longer(cols = c(Control:Natural), names_to = "grazing", values_to = "consumption")

    }
  ),


  # Prepare cover data
  # keep 3 control plots
  tar_target(
    name = cover_total,
    command = cover_raw %>%
      # first and last year
      filter(year %in% c(2019, 2022)) |>
      # remove 150 kg N
      filter(Namount_kg_ha_y != 150) |>
      # fix species names
      mutate(species = case_when(str_detect(species, "Antennaria") ~ "Antennaria sp",
                                 species == "Carex capillaris cf" ~ "Carex capillaris",
                                 species == "Carex leporina cf" ~ "Carex leporina",
                                 species == "Carex nigra cf" ~ "Carex nigra",
                                 species == "Carex vaginata cf" ~ "Carex vaginata",
                                 species == "Epilobium anagallidifolium cf" ~ "Epilobium anagallidifolium cf",
                                 species == "Erigeron uniflorus cf" ~ "Erigeron uniflorus",
                                 species == "Euphrasia wetsteinii cf" ~ "Euphrasia wetsteinii",
                                 str_detect(species, "Luzula") ~ "Luzula sp",
                                 str_detect(species, "Pyrola") ~ "Pyrola sp",
                                 TRUE ~ species)) |>
      # remove very rare species that occur less than 2 times in the dataset
      group_by(species) |>
      mutate(n = n()) |> #distinct(species, n) |> arrange(species) |> print(n = Inf)
      # Remove Carex rupestris and norvegica cf, Carex sp, because they are very uncertain
      filter(!str_detect(species, "Unknown"),
             !species %in% c("Carex rupestris", "Carex rupestris cf",
                             "Carex norvegica cf", "Carex sp")) |>
      ungroup() |>
      # sum cover of duplicates species
      tidylog::summarise(cover = sum(cover), .by = c(turfID, warming, grazing,
                                                     Nlevel, species, year, origSiteID, origBlockID, origPlotID, destSiteID, destBlockID, destPlotID, Namount_kg_ha_y)) |>

      # prettify
      mutate(origSiteID = recode(origSiteID, "Liahovden" = "Alpine", "Joasete" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
             # make grazing numeric
             grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = NA_character_),
                    grazing_num = as.numeric(grazing_num)) %>%

      # log transform Nitrogen
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1))

  ),

  # average controls
  # tar_target(
  #   name = cover,
  #   command = cover_total |>
  #
  #     # take average cover of 3 control plots
  #     group_by(year, origSiteID, destSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, grazing_num, species) |>
  #     summarise(cover = mean(cover)) |>
  #
  #     # add taxon information
  #     left_join(sp_list |>
  #                 mutate(species = paste(genus, species, sep = " ")), by = "species") |>
  #     # fix NA's in functional group
  #     mutate(functional_group = case_when(species == "Carex nigra" ~ "graminoid",
  #                                         species %in% c("Oxytropa laponica", "Galium verum", "Veronica officinalis", "Erigeron uniflorus", "Epilobium anagallidifolium") ~ "forb",
  #                                         functional_group == "pteridophyte" ~ "forb",
  #                                         grepl("Carex", species) ~ "sedge",
  #                                         TRUE ~ functional_group)) |>
  #     ungroup() |>
  #     select(-genus, -family)
  # ),

  # Change in functional group cover
  # tar_target(
  #   name = functional_group_cover,
  #   command = {
  #     all <- cover %>%
  #     # remove shrubs, sedge from sub-alpine and legumes from alpine (too few species)
  #     filter(functional_group != "shrub",
  #            !(functional_group == "sedge" & origSiteID == "Sub-alpine"),
  #            !(functional_group == "legume" & origSiteID == "Alpine"))
  #
  #     # graminoids including sedge, forbs, including legumes and sedges, legumes separate
  #     bind_rows(all = all |>
  #                 mutate(functional_group = case_when(functional_group == "sedge" ~ "graminoid",
  #                                                     functional_group == "legume" ~ "forb",
  #                                                     TRUE ~ functional_group)),
  #               sedge = all |>
  #                 filter(functional_group == "sedge"),
  #               legume = all |>
  #                 filter(functional_group == "legume"),
  #               .id = "group") |>
  #       group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, grazing_num, group, functional_group, year) %>%
  #     summarise(cover = sum(cover)) %>%
  #     pivot_wider(names_from = year, values_from = cover) %>%
  #     # Fun groups that do not exist in one year => 0
  #     # calculate difference between years
  #     mutate(`2019` = if_else(is.na(`2019`), 0, `2019`),
  #            `2022` = if_else(is.na(`2022`), 0, `2022`),
  #            delta = `2022` - `2019`) %>%
  #     ungroup()
  #
  #   }
  #
  # ),

  # prep height
  tar_target(
    name = height,
    command = {

      # replace height values of 0 with average from previous year
      height_estimation <- height_raw |>
        filter(vegetation_layer == "Vascular plant layer") |>
        filter(turfID %in% c("133 WN4C 186", "33 AN10I 33", "35 AN10C 35", "70 AN9C 70")) |>
        filter(height > 0) |>
        group_by(turfID) |>
        summarise(height_est = mean(height),
                  year = 2022,
                  vegetation_layer = "Vascular plant layer")

      height_raw %>%
        # first and last year
        filter(year %in% c(2019, 2022)) |>
        # add meta data
        left_join(metaTurfID, by = "turfID") |>
        # remove 150 kg N
        filter(Namount_kg_ha_y != 150) |>

        # prettify
        mutate(origSiteID = recode(origSiteID, "Liahovden" = "Alpine", "Joasete" = "Sub-alpine"),
               origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
               grazing = factor(grazing, levels = c("C", "M", "I", "N")),
               grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
               warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
               # make grazing numeric
               grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = NA_character_),
               grazing_num = as.numeric(grazing_num)) |>
        # replace 0 heights
        left_join(height_estimation, by = c("turfID", "year", "vegetation_layer")) |>
        mutate(height = if_else(!is.na(height_est), height_est, height)) |>
        select(-height_est) |>

        # take average cover of 3 control plots
        group_by(year, origSiteID, destSiteID, warming, grazing, Namount_kg_ha_y, grazing_num, vegetation_layer) |>
        summarise(height = mean(height)) |>
        ungroup() |>
        pivot_wider(names_from = year, values_from = height) |>
        # Fun groups that do not exist in one year => 0
        # calculate difference between years
        mutate(`2019` = if_else(is.na(`2019`), 0, `2019`),
               `2022` = if_else(is.na(`2022`), 0, `2022`),
               delta = `2022` - `2019`) |>
        ungroup()

    }

  ),


  # diversity
  tar_target(
    name = biomass_div,
    command = {

      # get change in diversity (delta) and log ratio
      dat <- cover_total %>%
        ungroup() |>
        group_by(origSiteID, year, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log) %>%
        summarise(richness = n(),
                  diversity = diversity(cover),
                  evenness = diversity/log(richness)) %>%
        pivot_longer(cols = c(richness, diversity, evenness), names_to = "diversity_index", values_to = "value") |>
        pivot_wider(names_from = year, values_from = value) %>%
        mutate(delta = `2022` - `2019`,
               log_ratio = log(`2022`/`2019`),
               final = `2022`) |>
        select(-c(`2019`, `2022`)) |>
        ungroup() |>
        pivot_wider(names_from = diversity_index, values_from = c(delta, log_ratio, final)) |>
        # add standing biomass
        left_join(standing_biomass_back |>
                    pivot_wider(names_from = year, values_from = standing_biomass) |>
                    mutate(delta_bio = `2022` - `2019`,
                           log_ratio_bio = log(`2022`/`2019`),
                           final_bio = `2022`) |>
                    select(-c(`2019`, `2022`)),
                  by = c('origSiteID', 'warming', "grazing", "Nlevel", 'Namount_kg_ha_y', 'Nitrogen_log')) |>
        mutate(grazing_num = as.numeric(recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = "2")),
               origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")))

    }

  ),

  ### bootstrapping traits

  # trait impute
  tar_target(
    name = trait_impute,
    command = make_trait_impute(cover_total, trait_raw)
  ),

  # trait impute
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute)
  )

)
