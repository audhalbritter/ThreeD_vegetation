download_plan <- list(

  #download_data

  # climate
  tar_target(
    name = climate_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_clean_microclimate_2019-2022.csv.zip",
                        path = "data",
                        remote_path = "Climate")
  ),

  # unzip and delete zip file
  tar_target(
    name = climate_unzip,
    command =  {
      unzip(climate_download, exdir = "data")

      zip <- "data/THREE-D_clean_microclimate_2019-2022.csv.zip"
      unzip <- "data/THREE-D_clean_microclimate_2019-2022.csv"
      #Check its existence
      if (file.exists(zip) & file.exists(unzip)) {
        #Delete file if it exists
        file.remove(zip)
      }
      unzip
    },
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
                        file = "Three-D_clean_biomass_2020-2022.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # productivity
  tar_target(
    name = productivity_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_productivity_2022.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # cover
  tar_target(
    name = cover_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_cover_2019-2022_new.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # height
  tar_target(
    name = height_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_height_2019_2020.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # soil
  tar_target(
    name = soil_download,
    command =  get_file(node = "pk4bg",
                        file = "THREE-D_Soil_2019-2020.csv",
                        path = "data",
                        remote_path = "Soil"),
    format = "file"
  ),

  # traits
  tar_target(
    name = trait_download,
    command =  get_file(node = "fcbw4",
                        file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
                        path = "data",
                        remote_path = "i. trait_data"),
    format = "file"
  ),



  # import and transform in data
  # climate
  tar_target(
    name = climate_raw,
    command = read_csv(climate_unzip)
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

  # productivity
  tar_target(
    name = productivity_raw,
    command = read_csv(file = productivity_download)
  ),

  # cover
  tar_target(
    name = cover_raw,
    command = read_csv(file = cover_download)
  ),

  # height
  tar_target(
    name = height_raw,
    command = read_csv(file = height_download)
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
    command = read_csv(file = trait_download)
  ),

  # traits
  tar_target(
    name = trait_raw,
    command = read_delim(file = trait_download, delim = ",")
  )

)

