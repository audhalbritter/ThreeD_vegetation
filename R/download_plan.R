download_plan <- list(

  #download_data

  # climate
  tar_target(
    name = climate_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_clean_microclimate_2019-2021.csv.zip",
                        path = "data",
                        remote_path = "Climate"),
    format = "file"
  ),

  tar_target(
    name = gridded_climate_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE_D_GriddedDailyClimateData2008-2022.csv",
                        path = "data",
                        remote_path = "Climate"),
    format = "file"
  ),

  # biomass
  tar_target(
    name = biomass_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_clean_biomass_2020-2021.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # vegetation
  # tar_target(
  #   name = cover_download,
  #   command =  get_file(node = "pk4bg",
  #                       file = "THREE-D_Cover_2019-2021.csv",
  #                       path = "data",
  #                       remote_path = "Vegetation"),
  # format = "file"
  # ),

  # soil
  tar_target(
    name = soil_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_Soil_2019-2020.csv",
                        path = "data",
                        remote_path = "Soil"),
    format = "file"
  ),

  tar_target(
  name = meta_soil_download,
  command =  get_file(node = "pk4bg",
                      file = "THREE-D_PlotLevel_Depth_2019.csv",
                      path = "data",
                      remote_path = "Soil"),
  format = "file"
  ),


  # import and transform in data

  # climate
  tar_target(
    name = climate_unzip,
    command = unzip(climate_download, exdir = "data")
  ),

  tar_target(
    name = climate_raw,
    command = read_csv(file = climate_unzip[1])
  ),

  tar_target(
    name = gridded_climate_raw,
    command = read_csv(file = gridded_climate_download)
  ),

  # biomass
  tar_target(
    name = biomass_raw,
    command = read_csv(file = biomass_download)
  ),

  # cover
  tar_target(
    name = cover_raw,
    command = read_csv("data/THREE-D_Cover_2019-2021.csv")
  ),

  tar_target(
    name = sp_list,
    command = read_csv(file = "data/species_list.csv")
  ),

  tar_target(
    name = metaTurfID,
    command = read_excel(path = "data/metaTurfID.xlsx")
  ),

  # soil
  tar_target(
    name = soil_raw,
    command = read_delim(file = soil_download, delim = ",")
  ),

  tar_target(
    name = meta_soil_raw,
    command = read_csv(file = meta_soil_download)
  )

)

