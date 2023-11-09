# try to make as few targets as possible as each target is cached.
# With many intermediate steps, it uses a lot of disk space.

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
  tar_target(
    name = biomass,
    command = {
      biomass_raw %>%
        # calculate area in m2 and scale biomass to m2
        mutate(area_m2 = area_cm2 / 10000,
               biomass_scaled = biomass / area_m2
        ) %>%
        # only useful data for last 2 years
        filter(year %in% c(2022)) %>%

        # log transform Nitrogen
        mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

        # prettify
        mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
               origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
               grazing = factor(grazing, levels = c("C", "M", "I", "N")),
               grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
               warming = recode(warming, "A" = "Ambient", "W" = "Warming")) |>

        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
               grazing_num = as.numeric(grazing_num))

    }),


  # annual productivity
  tar_target(
    name = standing_biomass,
    command = {
      biomass |>
        # get peak biomass
        filter(grazing == "Control" & cut == 3| grazing %in% c("Medium", "Intensive") & cut == 4) |>

        # Calculate mean of 0 kg N per m2 y
        ungroup() |>
        group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, fun_group, year) |>
        summarise(standing_biomass = mean(biomass_scaled))

    }),


  # annual productivity
  tar_target(
    name = productivity,
    command = {
      biomass |>
  # summarise the cuts to get annual biomass/productivity
  group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, fun_group, year) %>%
    summarise(productivity = sum(biomass_scaled)) %>%

    # Calculate mean of 0 kg N per m2 y
    ungroup() |>
    group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, fun_group, year) |>
    summarise(productivity = mean(productivity))

    }),


  # prep height
  tar_target(
    name = height,
    command = height_raw %>%
      # first and last year
      filter(year %in% c(2019, 2022)) |>
      # add meta data
      left_join(metaTurfID, by = "turfID") |>

      # prettify
      mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
             # make grazing numeric
             grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = NA_character_),
             grazing_num = as.numeric(grazing_num)) |>

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
  ),



  # prep cover
  tar_target(
    name = cover,
    command = cover_raw %>%
      # first and last year
      filter(year %in% c(2019, 2022)) |>
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

      # prettify
      mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
             # make grazing numeric
             grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = NA_character_),
                    grazing_num = as.numeric(grazing_num)) %>%

      # add Namount variable
      left_join(metaTurfID %>%
                  distinct(Nlevel, Namount_kg_ha_y), by = "Nlevel") %>%
      # log transform Nitrogen
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

      # take average cover of 3 control plots
      group_by(year, date, origSiteID, destSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, grazing_num, species) |>
      summarise(cover = mean(cover)) |>

      # add taxon information
      left_join(sp_list |>
                  mutate(species = paste(genus, species, sep = " ")), by = "species") |>
      # fix NA's in functional group
      mutate(functional_group = case_when(species == "Carex nigra" ~ "graminoid",
                                          species %in% c("Oxytropa laponica", "Galium verum", "Veronica officinalis", "Erigeron uniflorus", "Epilobium anagallidifolium") ~ "forb",
                                          functional_group == "pteridophyte" ~ "forb",
                                          grepl("Carex", species) ~ "sedge",
                                          TRUE ~ functional_group)) |>
      ungroup() |>
      select(-genus, -family)
  ),

  # Change in functional group cover
  tar_target(
    name = functional_group_cover,
    command = cover %>%
      # remove shrubs, sedge from sub-alpine and legumes from alpine (too few species)
      filter(functional_group != "shrub",
             !(functional_group == "sedge" & origSiteID == "Sub-alpine"),
             !(functional_group == "legume" & origSiteID == "Alpine")) |>
      group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, grazing_num, functional_group, year) %>%
      summarise(cover = sum(cover)) %>%
      pivot_wider(names_from = year, values_from = cover) %>%
      # Fun groups that do not exist in one year => 0
      # calculate difference between years
      mutate(`2019` = if_else(is.na(`2019`), 0, `2019`),
             `2022` = if_else(is.na(`2022`), 0, `2022`),
             delta = `2022` - `2019`) %>%
      # remove highest N level
      #filter(Namount_kg_ha_y != 150) |>
      ungroup()
  ),


  # community structure
  tar_target(
    name = comm_structure,
    command = community_structure_raw  |>
      # first and last year and relevant groups
      filter(year %in% c(2019, 2022),
             !functional_group %in% c("Wool", "Poop", "SumofCover")) |>

      # prettify
      mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
             # make grazing numeric
             grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = NA_character_),
             grazing_num = as.numeric(grazing_num)) |>

      # add Namount variable
      left_join(metaTurfID %>%
                  distinct(Nlevel, Namount_kg_ha_y), by = "Nlevel") |>

      # log transform Nitrogen
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

      # take average cover of 3 control plots
      group_by(year, origSiteID, destSiteID, warming, grazing, Nitrogen_log, Namount_kg_ha_y, grazing_num, functional_group) |>
      summarise(cover = mean(cover)) |>
      ungroup() |>
      pivot_wider(names_from = year, values_from = cover) |>
      # Fun groups that do not exist in one year => 0
      # calculate difference between years
      mutate(`2019` = if_else(is.na(`2019`), 0, `2019`),
             `2022` = if_else(is.na(`2022`), 0, `2022`),
             delta = `2022` - `2019`)
  ),

  # diversity
  tar_target(
    name = diversity,
    command = cover %>%
      group_by(origSiteID, year, warming, grazing, grazing_num, Namount_kg_ha_y, Nitrogen_log) %>%
      summarise(richness = n(),
                diversity = diversity(cover),
                evenness = diversity/log(richness)) %>%
      pivot_longer(cols = c(richness, diversity, evenness), names_to = "diversity_index", values_to = "value") |>

      # average for 0 kg N treatment
      ungroup() |>
      group_by(origSiteID, year, warming, grazing, grazing_num, Namount_kg_ha_y, Nitrogen_log, diversity_index) |>
      summarise(value = mean(value)) |>
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(delta = `2022` - `2019`) |>
      # make grazing numeric
      mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
             grazing_num = as.numeric(grazing_num)) |>
      ungroup()
  ),

  # productivity and diversity
  tar_target(
    name = biomass_diversity,
    command = diversity |>
    filter(grazing != "Natural") |>
    left_join(standing_biomass |>
                filter(fun_group != "litter") |>
                ungroup() |>
                group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num) |>
                summarise(biomass = sum(standing_biomass)),
              by = c('origSiteID', "grazing", "grazing_num", 'warming', 'Namount_kg_ha_y', 'Nitrogen_log'))
  )

)
