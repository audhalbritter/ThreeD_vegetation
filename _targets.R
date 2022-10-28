library("targets")
library("tarchetypes")

tar_option_set(packages = c("sf", "dataDownloader", "tidyverse", "readxl", "vegan", "viridis", "lubridate", "MuMIn", "performance", "broom", "DBI", "RSQLite", "dataDocumentation"))
#"rjt.misc",

# source other scripts
#source("R/load_libraries.R")
source("R/functions/make_climate_figures.R")
source("R/functions/make_vegetation_figures.R")
source("R/functions/make_analysis.R")
#source("R/functions/extinction_colonization.R")

# source target plans - can also construct plans directly in this file.
source("R/download_plan.R")
source("R/tranformation_plan.R")
source("R/analysis_plan.R")
source("R/figure_plan.R")
# source("R/manuscript_plan.R")


#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  analysis_plan,
  figure_plan
  # manuscript_plan
)
