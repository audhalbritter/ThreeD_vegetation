library("targets")
source("other_scripts/load_libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
tar_make()
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

fs::file_show("manuscript/manuscript.pdf")#display pdf

# remotes::install_github("claudiozandonella/trackdown",
#                         build_vignettes = TRUE)
library(trackdown)
# copy client number
trackdown_auth_configure(path = "")
trackdown::upload_file(file = "test_file.qmd",
                       hide_code = TRUE,
                       path_output = "test_file.html")
