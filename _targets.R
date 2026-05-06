library("targets")
library("tarchetypes")

#install.packages("httpgd_2.0.4.tar.gz", repos = NULL, type = "source")

tar_option_set(packages = c("sf", "dataDownloader", "here", "tidyverse", "readxl", "janitor", "vegan", "ggvegan", "viridis", "performance", "broom", "DBI", "RSQLite", "dataDocumentation", "patchwork", "ggh4x", "gt", "ggpubr", "ggraph", "tidygraph", "quarto", "piecewiseSEM", "traitstrap", "data.table", "MetBrewer", "glue", "ggnetwork", "cowplot", "colorBlindness"))
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
  figure_plan,
  si_figure_plan,
  manuscript_plan
)