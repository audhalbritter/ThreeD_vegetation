download_plan <- list(

  #download_data

  # climate
  # tar_target(
  #   name = climate_download,
  #   command =  get_file(node = "pk4bg",
  #                       file = "THREE-D_clean_microclimate_2019-2021.csv.zip",
  #                       path = "data",
  #                       remote_path = "Climate"),
  #   format = "file"
  # ),

  tar_target(
    name = gridded_climate_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE_D_GriddedDailyClimateData2008-2022.csv",
                        path = "data",
                        remote_path = "Climate"),
    format = "file"
  ),

  # biomass
  # tar_target(
  #   name = biomass_download,
  #   command =  get_file(node = "pk4bg",
  #                       file = "Three-D_clean_biomass_2020-2022.csv",
  #                       path = "data",
  #                       remote_path = "Vegetation"),
  #   format = "file"
  # ),

  # productivity
  tar_target(
    name = productivity_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_productivity_2022.csv",
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

  # tar_target(
  # name = meta_soil_download,
  # command =  get_file(node = "pk4bg",
  #                     file = "THREE-D_PlotLevel_Depth_2019.csv",
  #                     path = "data",
  #                     remote_path = "Soil"),
  # format = "file"
  # ),


  # import and transform in data

  # climate
  # tar_target(
  #   name = climate_unzip,
  #   command = unzip(climate_download, exdir = "data")
  # ),

  tar_target(
    name = climate_raw,
    #command = read_csv(file = climate_unzip[1])
    command = read_csv(file = "data/THREE-D_clean_microclimate_2019-2022.csv")
  ),

  tar_target(
    name = gridded_climate_raw,
    command = read_csv(file = gridded_climate_download)
  ),

  # biomass
  tar_target(
    name = biomass_raw,
    command = read_csv(file = "data/Three-D_clean_biomass_2020-2022.csv")
  ),

  # productivity
  tar_target(
    name = productivity_raw,
    command = read_csv(file = productivity_download)
  ),

  # height
  tar_target(
    name = height_raw,
    command = read_csv(file = "data/Three-D_clean_height_2019_2020.csv")
  ),

  # cover
  tar_target(
    name = cover_raw,
    command = read_csv("data/Three-D_clean_cover_2019-2022_new.csv")
  ),

  # community structure
  tar_target(
    name = community_structure_raw,
    command = read_csv("data/Three-D_clean_community_structure_2019-2022_new.csv")
  ),

  tar_target(
    name = sp_list,
    command = read_csv(file = "data/Three-D_clean_taxonomy.csv")
  ),

  tar_target(
    name = metaTurfID,
    command = create_threed_meta_data()
  ),

  # soil
  tar_target(
    name = soil_raw,
    command = read_delim(file = soil_download, delim = ",")
  )#,

  # tar_target(
  #   name = meta_soil_raw,
  #   command = read_csv(file = meta_soil_download)
  # )

)

