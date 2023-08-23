### make outputs

# Climate
tar_load(annual_climate_figure)
ggsave(filename = "output/annual_climate.png", annual_climate_figure, dpi = 300, width = 8, height = 7)

tar_load(daily_climate_figure)
ggsave(filename = "output/climate.png", daily_climate_figure, dpi = 300, width = 10, height = 7)

tar_load(climate_treatment_figure)
ggsave(filename = "output/climate_treatment.png", climate_treatment_figure, dpi = 300, width = 10, height = 7)

# Vegetation
# prodctivity
tar_load(productivity_figure)
ggsave(filename = "output/productivity.png", productivity_figure, dpi = 300, width = 4, height = 5)

# Cover
tar_load(cover_figure)
ggsave(cover_figure, filename = "output/cover.png", dpi = 300, width = 6, height = 5, bg = "white")

tar_load(cover_bryo_figure)
ggsave(cover_bryo_figure, filename = "output/cover_bryo_figure.png", dpi = 300, width = 8, height = 6, bg = "white")

# Diversity
tar_load(diversity_figure)
ggsave(diversity_figure, filename = "output/diversity_figure.png", dpi = 300, width = 8, height = 6, bg = "white")



### SI

tar_load(biomass_fun_group)
ggsave(filename = "output/biomass_fun_group.png", biomass_fun_group, dpi = 300, width = 10, height = 8, bg = "white")

tar_load(productivity_natural_figure)
ggsave(filename = "output/productivity_natural_figure.png", productivity_natural_figure, dpi = 300, width = 6, height = 4)


tar_load(cover_CN_figure)
ggsave(cover_CN_figure, filename = "output/cover_CN.png", dpi = 300, width = 6, height = 5, bg = "white")

tar_load(diversity_CN_figure)
ggsave(diversity_CN_figure, filename = "output/diversity_CN_figure.png", dpi = 300, width = 8, height = 6, bg = "white")
