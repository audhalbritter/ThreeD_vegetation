library("targets")
library("tarchetypes")

tar_option_set(packages = c("sf", "dataDownloader", "tidyverse", "readxl", "janitor", "vegan", "viridis", "lubridate", "MuMIn", "performance", "broom", "DBI", "RSQLite", "dataDocumentation", "patchwork", "wesanderson", "ggh4x", "gt", "ggpubr", "lavaan", "tidySEM", "quarto", "piecewiseSEM", "traitstrap"))
# sf, DBI, RSQLite, MuMin?

# source other scripts
tar_source()


#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  analysis_plan,
  piecewiseSEM_plan,
  trait_plan,
  si_analysis_plan,
  si_SEM_final_plan,
  si_SEM_change_plan,
  figure_plan,
  si_figure_plan,
  manuscript_plan
)