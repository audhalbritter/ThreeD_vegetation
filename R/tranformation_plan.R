# try to make as few targets as possible as each target is cached.
# With many intermediate steps, it uses a lot of disk space.

tranformation_plan <- list(

  # climate data
  tar_target(
    name = daily_temp,
    command = climate %>%
      tidylog::filter(!is.na(air_temperature),
                      # only controls
                      Nlevel %in% c(1, 2, 3),
                      grazing == "C") %>%
      mutate(date = dmy(format(date_time, "%d.%b.%Y"))) %>%
      group_by(date, destSiteID, warming, turfID) %>%
      summarise(air_temperature = mean(air_temperature)) %>%
      # summer temp
      mutate(month = month(date)) %>%
      filter(month %in% c(5, 6, 7, 8)) %>%
      group_by(destSiteID, warming) %>%
      summarise(mean = mean(air_temperature),
                se = sd(air_temperature)/sqrt(n()))
  ),

  # cover
  tar_target(
    name = cover_data,
    command = cover %>%
      left_join(sp_list, by = "species") %>%
      group_by(turfID, origBlockID, origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, functional_group, year) %>%
      summarise(cover = sum(cover)) %>%
      pivot_wider(names_from = year, values_from = cover) %>%
      mutate(delta = `2021` - `2019`) %>%
      #filter(functional_group %in% c("forb", "graminoid")) %>%
      ungroup()
  ),

  # richness
  tar_target(
    name = diversity_data,
    command = cover %>%
      ungroup() %>%
      group_by(turfID, origBlockID, origSiteID, year, warming, grazing, Namount_kg_ha_y, Nitrogen_log) %>%
      summarise(richness = n(),
                diversity = diversity(cover),
                evenness = diversity/log(richness)) %>%
      pivot_longer(cols = c(richness, diversity, evenness), names_to = "diversity_index", values_to = "value") |>
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(delta = `2021` - `2019`) %>%
      ungroup()
  ),

  # biomass
  tar_target(
    name = biomass_data,
    command = {
      biomass_raw %>%
        # scale biomass to 50 x 50 cm plot
        mutate(biomass_scaled = if_else(area_cm2 == 2500, biomass, biomass * 2500 / area_cm2)) %>%
        filter(year == 2021) %>%
        # summarise to get annual biomass/productivity
        group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, grazing, fun_group) %>%
        summarise(productivity = sum(biomass_scaled)) %>%
        mutate(origSiteID = recode(origSiteID, "Lia" = "High alpine", "Joa" = "Alpine"),
               origSiteID = factor(origSiteID, levels = c("High alpine", "Alpine")),
               grazing = factor(grazing, levels = c("C", "M", "I", "N")),
               grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
               warming = recode(warming, "A" = "ambient", "W" = "warming"))

    })

)
