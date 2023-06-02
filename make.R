library("targets")


# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
tar_make()

# view pipeline and show which targets are out of date
tar_visnetwork()

fs::file_show("manuscript/manuscript.pdf")#display pdf

remotes::install_github("claudiozandonella/trackdown",
                        build_vignettes = TRUE)
library(trackdown)
trackdown::upload_file(file = "Main.qmd",
                       hide_code = TRUE,
                       path_output = "Main.html")
