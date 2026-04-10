library("targets")
source("other_scripts/load_libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
targets::tar_make()

# tar_make(-manuscript, -si)
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

# fs::file_show("manuscript/manuscript.pdf")#display pdf

# remotes::install_github("claudiozandonella/trackdown",
#                         build_vignettes = TRUE)
# library(trackdown)
# # copy client number
# trackdown_auth_configure(path = "")
# trackdown::upload_file(file = "test_file.qmd",
#                        hide_code = TRUE,
#                        path_output = "test_file.html")



ggsave("output/Fig3_biomass_div.png", bio_div_figure, dpi = 300, height = 6, width = 7)
ggsave("output/Fig4_sem_fig.png", cut_final_diversity[[1]], dpi = 300, height = 4, width = 10)
ggsave("output/Fig5_standingB_div.png", standingB_div_final_figure, dpi = 300, height = 5, width = 8)
# ggsave("output/Fig6_toenail.png", , dpi = 300, height = 12, width = 8)
