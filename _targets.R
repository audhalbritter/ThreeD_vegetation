library("targets")
library("tarchetypes")

tar_option_set(packages = c("sf", "dataDownloader", "tidyverse", "readxl", "vegan", "viridis", "lubridate", "MuMIn", "performance", "broom", "DBI", "RSQLite", "dataDocumentation", "patchwork", "wesanderson", "ggh4x", "gt", "ggpubr", "lavaan", "tidySEM", "quarto", "piecewiseSEM", "traitstrap"))
# sf, DBI, RSQLite, MuMin?

# source other scripts
tar_source()


#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  piecewiseSEM_plan,
  figure_plan,
  si_analysis_plan,
  si_figure_plan,
  analysis_plan
  # nutrient_plan,
  # #manuscript_plan,
  # output_plan
)
