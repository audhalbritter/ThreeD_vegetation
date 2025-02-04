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

ggsave("output/Fig2_sem_fig.png", sem_fig, dpi = 300, height = 4, width = 10)
ggsave("output/Fig3_biomass_div.png", standingB_div_figure, dpi = 300, height = 6, width = 6)
ggsave("output/Fig4_grazing_div.png", grazing_div_figure, dpi = 300, height = 6, width = 6)


ggsave("output/FigSx_origin.png", SEM_origin_fig, dpi = 300, height = 8, width = 10)
ggsave("output/FigSx_div.png", SEM_diversity_fig, dpi = 300, height = 8, width = 10)

ggsave("output/consumption.png", productivity_consumption_figure, dpi = 300, height = 4, width = 8)
