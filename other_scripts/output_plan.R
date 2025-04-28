# Make output plan

output_plan <- list(

  ### STATS TABLES
  # biomass
  tar_target(
    name = biomass_stats,
    command = make_biomass_stats(biomass_anova_table)
  ),

  tar_target(
    name = biomass_save,
    command = biomass_stats |>
      gtsave("output/biomass_stats.png", expand = 10)
  ),

  # cover
  tar_target(
    name = cover_stats,
    command = make_cover_stats(cover_anova_table)
  ),

  tar_target(
    name = cover_save,
    command = cover_stats |>
      gtsave("output/cover_stats.png", expand = 10)
  ),

  # diversity
  tar_target(
    name = diversity_stats,
    command = make_diversity_stats(diversity_anova_table)
  ),

  tar_target(
    name = diversity_save,
    command = diversity_stats |>
      gtsave("output/diversity_stats.png", expand = 10)
  ),

  # APPENDIX

  # climate
  tar_target(
    name = climate_stats,
    command = make_climate_stats(climate_anova_table)
  ),

  tar_target(
    name = climate_save,
    command = climate_stats |>
      gtsave("output/climates_stats.png", expand = 10)
  ),

  # nutrients
  tar_target(
    name = nutrients_stats,
    command = make_nutrient_stats2(nutrients_anova_table)
  ),

  tar_target(
    name = nutrients_save,
    command = nutrients_stats |>
      gtsave("output/nutrients_stats.png", expand = 10)
  ),

    # CN cover
    tar_target(
      name = CN_cover_stats,
      command = make_cover_stats(cover_CN_anova_table)
    ),

  tar_target(
    name = CN_cover_save,
    command = CN_cover_stats |>
      gtsave("output/CN_cover_stats.png", expand = 10)
  ),

  # CN diversity
  tar_target(
    name = CN_diversity_stats,
    command = make_diversity_stats(diversity_CN_anova_table)
  ),

  tar_target(
    name = CN_diversity_save,
    command = CN_diversity_stats |>
      gtsave("output/CN_diversity_stats.png", expand = 10)
  )

)
