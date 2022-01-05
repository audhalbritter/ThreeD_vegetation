download_plan <- list(

  #download_data
  tar_target(
    name = raw_cover,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_Cover_2019_2020.csv",
                        path = "data",
                        remote_path = "Vegetation")
  ),
  tar_target(
    name = raw_climate,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_clean_microclimate_2019-2021.csv.zip",
                        path = "data",
                        remote_path = "Climate")
  ),
  tar_target(
    name = raw_soil,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_Soil_2019-2020.csv",
                        path = "data",
                        remote_path = "Soil")
  ),
  tar_target(
  name = raw_soil_meta,
  command =  get_file(node = "pk4bg",
                      file = "THREE-D_PlotLevel_Depth_2019.csv",
                      path = "data",
                      remote_path = "Soil")
  ),

  # read in data
  tar_target(
    name = cover,
    command = read_csv(file = "data/THREE-D_Cover_2019-2021.csv") %>%
      filter(year %in% c(2019, 2021),
             # just for now!!!
             !str_detect(species, "Carex"),
             !Nlevel %in% c(3, 2)) %>%
      mutate(origSiteID = recode(origSiteID, "Lia" = "High alpine", "Joa" = "Alpine"),
             origSiteID = factor(origSiteID, levels = c("High alpine", "Alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I", "N")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural")) %>%
      left_join(metaTurfID %>% distinct(Nlevel, Namount_kg_ha_y), by = "Nlevel")
  ),

  tar_target(
    name = raw_climate2,
    command = unzip("data/THREE-D_clean_microclimate_2019-2021.csv.zip", exdir = "data")
  ),

  tar_target(
  name = climate,
  command = read_csv(file = "data/THREE-D_clean_microclimate_2019-2021.csv")
  ),

  tar_target(
    name = sp_list,
    command = read_csv(file = "data/species_list.csv")
  ),

  tar_target(
    name = metaTurfID,
    command = read_excel(path = "data/metaTurfID.xlsx")
  ),

  tar_target(
    name = soil,
    command = read_delim(file = "data/THREE-D_Soil_2019-2020.csv", delim = ",")
  ),

  tar_target(
    name = meta_soil,
    command = read_csv(file ="data/THREE-D_PlotLevel_Depth_2019.csv")
  )

)

