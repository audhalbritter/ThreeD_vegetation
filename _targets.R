library("targets")
library("tarchetypes")

tar_option_set(packages = c("sf", "dataDownloader", "tidyverse", "readxl", "vegan", "viridis", "lubridate", "MuMIn", "performance", "broom", "DBI", "RSQLite", "dataDocumentation", "patchwork", "wesanderson", "ggh4x", "gt", "ggpubr", "lavaan", "tidySEM", "quarto"))
# sf, DBI, RSQLite, MuMin?

# source other scripts
tar_source()


#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  analysis_plan,
  sem_plan,
  figure_plan,
  #manuscript_plan,
  si_analysis_plan,
  si_figure_plan,
  nutrient_plan,
  output_plan
)
