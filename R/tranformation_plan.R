# try to make as few targets as possible as each target is cached.
# With many intermediate steps, it uses a lot of disk space.

tranformation_plan <- list(

  # climate data
  tar_target(
    name = daily_temp,
    command = climate_raw %>%
      # daily temperature
      mutate(date = dmy(format(date_time, "%d.%b.%Y")),
             warming = recode(warming, A = "Ambient", W = "Warming")) %>%
      group_by(date, destSiteID, destBlockID, destPlotID, origSiteID, origBlockID, warming, Nlevel, grazing) %>%
      summarise(air_temperature = mean(air_temperature, na.rm = TRUE),
                ground_temperature = mean(ground_temperature, na.rm = TRUE),
                soil_temperature = mean(soil_temperature, na.rm = TRUE),
                soilmoisture = mean(soilmoisture, na.rm = TRUE)) |>
      pivot_longer(cols = c(air_temperature:soilmoisture), names_to = "variable", values_to = "value") |>
      filter(!is.na(value))
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
        filter(year == 2021) %>%

        # log transform Nitrogen
        mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

        # summarise the cuts to get annual biomass/productivity
        group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, fun_group) %>%
        summarise(productivity = sum(biomass_scaled)) %>%

        # Calculate mean of 0 kg N per m2 y
        ungroup() |>
        group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, fun_group) |>
        tidylog::summarise(productivity = mean(productivity)) |>

        mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
               origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
               grazing = factor(grazing, levels = c("C", "M", "I", "N")),
               grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
               warming = recode(warming, "A" = "Ambient", "W" = "Warming"))

    }),


  # prep cover
  tar_target(
    name = cover,
    command = cover_raw %>%
      # first and last year
      filter(year %in% c(2019, 2021),
             # just for now!!!
             !str_detect(species, "Carex")) %>%

      # prettify
      mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming")) %>%

      # add Namount variable
      left_join(metaTurfID %>%
                  distinct(Nlevel, Namount_kg_ha_y), by = "Nlevel") %>%
      # log transform Nitrogen
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

      # take average cover of 3 control plots
      group_by(year, date, origSiteID, origBlockID, origPlotID, destSiteID, destBlockID, destPlotID, turfID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, species) |>
      summarise(cover = mean(cover)) |>

      # add taxon information
      left_join(sp_list, by = "species")
  ),

  # Change in functional group cover
  tar_target(
    name = functional_group_cover,
    command = cover %>%
      group_by(turfID, origBlockID, origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, functional_group, year) %>%
      summarise(cover = sum(cover)) %>%
      pivot_wider(names_from = year, values_from = cover) %>%
      # Fun groups that do not exist in one year => 0
      # calculate difference between years
      mutate(`2019` = if_else(is.na(`2019`), 0, `2019`),
             `2021` = if_else(is.na(`2021`), 0, `2021`),
             delta = `2021` - `2019`) %>%
      ungroup()
  ),

  # richness
  tar_target(
    name = diversity,
    command = cover %>%
      group_by(turfID, origBlockID, origSiteID, year, warming, grazing, Namount_kg_ha_y, Nitrogen_log) %>%
      summarise(richness = n(),
                diversity = diversity(cover),
                evenness = diversity/log(richness)) %>%
      pivot_longer(cols = c(richness, diversity, evenness), names_to = "diversity_index", values_to = "value") |>
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(delta = `2021` - `2019`) %>%
      ungroup()
  )

)
