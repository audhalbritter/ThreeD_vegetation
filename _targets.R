library("targets")
library("tarchetypes")

tar_option_set(packages = c("rjt.misc", "sf"))

# source other scripts
source("R/load_libraries.R")

# source target plans - can also construct plans directly in this file.
source("R/download_plan.R")
source("R/tranformation_plan.R")
# source("R/analysis_plan.R")
source("R/figure_plan.R")
source("R/manuscript_plan.R")


#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  # analysis_plan,
  figure_plan,
  manuscript_plan
)
