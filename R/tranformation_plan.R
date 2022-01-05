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
      group_by(turfID, origBlockID, origSiteID, warming, grazing, Namount_kg_ha_y, functional_group, year) %>%
      summarise(cover = sum(cover)) %>%
      pivot_wider(names_from = year, values_from = cover) %>%
      mutate(delta = `2021` - `2019`) %>%
      filter(functional_group %in% c("forb", "graminoid")) %>%
      ungroup()
  ),

  # richness
  tar_target(
    name = richness_data,
    command = cover %>%
      ungroup() %>%
      group_by(turfID, origBlockID, origSiteID, year, warming, grazing, Namount_kg_ha_y) %>%
      summarise(richness = n()) %>%
      pivot_wider(names_from = year, values_from = richness) %>%
      mutate(delta = `2021` - `2019`) %>%
      ungroup()
  )

)
