download_plan <- list(

  #download_data

  # climate
  tar_target(
    name = climate_download,
    command = download_zenodo_file(
      source_name = "xiv_Three-D_clean_microclimate_2019-2022.csv"
    ),
    format = "file"
  ),

  # gridded climate - read from local file (not available online)
  # tar_target(
  #   name = gridded_climate_download,
  #   command =  get_file(node = "pk4bg",
  #                       file = "THREE_D_GriddedDailyClimateData2008-2022.csv",
  #                       path = here::here("data"),
  #                       remote_path = "Climate"),
  #   format = "file"
  # ),

  # biomass
  tar_target(
    name = biomass_download,
    command = download_zenodo_file(
      source_name = "iii_Three-D_clean_aboveground_biomass_2020-2022.csv"
    ),
    format = "file"
  ),

  # productivity
  tar_target(
    name = productivity_download,
    command = download_zenodo_file(
      source_name = "iv_Three-D_clean_aboveground_productivity_consumption_2022.csv"
    ),
    format = "file"
  ),

  # cover
  tar_target(
    name = cover_download,
    command = download_zenodo_file(
      source_name = "vii_Three-D_clean_community_cover_2019-2022.csv"
    ),
    format = "file"
  ),

  # height
  tar_target(
    name = height_download,
    command = download_zenodo_file(
      source_name = "ix_Three-D_clean_vegetation_structure_2019-2022.csv"
    ),
    format = "file"
  ),

  # species list
  tar_target(
    name = sp_list_download,
    command = download_zenodo_file(
      source_name = "vii_Three-D_clean_species_list.csv"
    ),
    format = "file"
  ),

  # traits
  tar_target(
    name = trait_download,
    command =  get_file(node = "fcbw4",
                        file = "PFTC6_clean_ElevationGradient_GlobalChangeExperiment_morphological_traits_2022.csv",
                        path = here::here("data"),
                        remote_path = "i. trait_data"),
    format = "file"
  ),


  # ellenberg values
  tar_target(
    name = ellenberg_download,
    command =  {
      url <- "https://zenodo.org/records/7427088/files/Indicator.values-tables-2022-11-07-Zenodo.v2.xlsx?download=1"
      destfile <- here::here("data", "ellenberg.xlsx")  # Local file name
      download.file(url, destfile, mode = "wb")
      # print path to file
      destfile
    },
    format = "file"
  ),
  
  # disturbance indicator values
  tar_target(
    name = disturbance_download,
    command =  {
      url <- "https://zenodo.org/records/7116957/files/disturbance_indicator_values.csv?download=1"
      destfile <- here::here("data", "disturbance_indicator_values.csv")  # Local file name
      download.file(url, destfile, mode = "wb")
      # print path to file
      destfile
    },
    format = "file"
  ),

  # import and transform in data
  # climate
  tar_target(
    name = climate_raw,
    command = fread(climate_download)
  ),

  tar_target(
    name = gridded_climate_raw,
    command = fread(file = here::here("data", "THREE_D_GriddedDailyClimateData2008-2022.csv"))
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
    command = read_csv(file = sp_list_download)
  ),

  tar_target(
    name = metaTurfID,
    command = create_threed_meta_data()
  ),

  # traits
  tar_target(
    name = trait_raw,
    command = read_delim(file = trait_download, delim = ",")
  ),

  # affinities
  tar_target(
    name = ellenberg_raw,
    command = read_xlsx(path = ellenberg_download,
                        sheet = "Tab-IVs-Tichy-et-al2023")
  ),

  # disturbance indicator values
  tar_target(
    name = disturbance_raw,
    command = read_csv(disturbance_download, show_col_types = FALSE)
  )

)

