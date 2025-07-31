library("targets")
library("tarchetypes")

#install.packages("httpgd_2.0.4.tar.gz", repos = NULL, type = "source")

tar_option_set(packages = c("sf", "dataDownloader", "tidyverse", "readxl", "janitor", "vegan", "viridis", "performance", "broom", "DBI", "RSQLite", "dataDocumentation", "patchwork", "ggh4x", "gt", "ggpubr", "tidySEM", "quarto", "piecewiseSEM", "traitstrap", "data.table", "MetBrewer"))
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