# Helper function to download files from Zenodo

download_zenodo_file <- function(source_name,
                                 dest_name = source_name,
                                 record_id = "17301125") {
  # Increase timeout for large files (e.g. microclimate ~1 GB)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))

  url <- paste0(
    "https://zenodo.org/records/",
    record_id,
    "/files/",
    source_name,
    "?download=1"
  )
  destfile <- here::here("data", dest_name)
  
  # Ensure the data directory exists
  data_dir <- here::here("data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # If file already exists, reuse it to avoid repeated downloads
  if (file.exists(destfile)) {
    message("File already exists, skipping download: ", normalizePath(destfile))
    return(destfile)
  }
  
  # Print where file is being saved (for debugging)
  message("Downloading to: ", destfile)
  
  download.file(url, destfile = destfile, mode = "wb")
  
  # Verify file exists and print final location
  if (file.exists(destfile)) {
    message("File successfully saved to: ", normalizePath(destfile))
  } else {
    warning("File may not have been saved correctly. Expected location: ", destfile)
  }
  
  return(destfile)
}
