### make outputs
library(tidyverse)
library(lubridate)
library(readxl)
library(vegan)
library(viridis)
library(performance)
library(broom)
library(dataDocumentation)
library(patchwork)
library(wesanderson)
library(ggh4x)
library(gt)


# Climate
tar_load(annual_climate_figure)
ggsave(filename = "output/annual_climate.png", annual_climate_figure, dpi = 300, width = 8, height = 7)

tar_load(daily_climate_figure)
ggsave(filename = "output/daily_climate.png", daily_climate_figure, dpi = 300, width = 10, height = 7)

tar_load(climate_figure)
ggsave(filename = "output/climate.png", climate_figure, dpi = 300, width = 10, height = 7)

# Vegetation
# productivity
tar_load(bio_cover_figure)
ggsave(filename = "output/biomass_cover.png", bio_cover_figure, dpi = 300, width = 8, height = 6)

# Diversity
tar_load(diversity_figure)
ggsave(diversity_figure, filename = "output/diversity_figure2.png", dpi = 300, width = 8, height = 10, bg = "white")



### SI
tar_load(biomass_fun_group)
ggsave(filename = "output/biomass_fun_group.png", biomass_fun_group, dpi = 300, width = 10, height = 8, bg = "white")

tar_load(productivity_consumption_figure)
ggsave(filename = "output/productivity_consumption_figure.png", productivity_consumption_figure, dpi = 300, width = 10, height = 4)

tar_load(productivity_figure)
ggsave(filename = "output/productivity_figure.png", productivity_figure, dpi = 300, width = 4, height = 4.5)

# biomass vs diversity
tar_load(bio_div_figure)
ggsave(bio_div_figure, filename = "output/bio_div_figure.png", dpi = 300, width = 6, height = 4, bg = "white")

tar_load(cover_CN_figure)
ggsave(cover_CN_figure, filename = "output/cover_CN.png", dpi = 300, width = 6, height = 5, bg = "white")

tar_load(diversity_CN_figure)
ggsave(diversity_CN_figure, filename = "output/diversity_CN_figure.png", dpi = 300, width = 8, height = 6, bg = "white")

tar_load(nutrient_figure)
ggsave(filename = "output/nutrient_figure.png", nutrient_figure, dpi = 300, width = 10, height = 6, bg = "white")
