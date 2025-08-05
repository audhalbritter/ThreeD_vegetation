# trait analysis

trait_plan <- list(

  # status: winners, losers, increasing, decreasing and stable species
  # mark status of species in cover data
  tar_target(
    name = cover_wl,
    command = get_winners_and_losers(cover_total)
  ),

  # impute traits for whole community (trait_fill)
  tar_target(
    name = trait_impute_all,
    command = make_trait_impute2(cover_wl |>
                                   filter(year == "2022" | year == 2019 & status == "extinction"),
                                 trait_raw,
                                 ellenberg)
  ),

  # bootstrap
  tar_target(
    name = trait_mean_all,
    command = make_bootstrapping(trait_impute_all)
  ),

  # make trait pca
  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean_all)
  ),

  # make trait pca plot
  # tar_target(
  #   name = trait_pca_plot,
  #   command = {
  #     trait_pca_plot <- autoplot(trait_pca,
  #                                axes = c(1, 2),
  #                                loadings = TRUE,
  #                                loadings.label = TRUE,
  #                                loadings.label.size = 3)
  #   }
  # ),

    # Trats ridgeline plot
    # warming
    tar_target(
      name = traits_warming_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean_all |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "salinity"),
                                                grazing != "Natural"), 
                                                group_var = "warming",
                                                custom_colors = treatment_palette[c(1, 2)],
                                                y_axis_label = "")
        add_significance_stars(base_plot, trait_statistical_analysis, "warming")
      }
    ),

    tar_target(
      name = traits_nitrogen_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean_all |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "salinity"),
                                                grazing != "Natural") |>
                                                mutate(Namount_kg_ha_y2 = as.factor(Namount_kg_ha_y)), 
                                                group_var = "Namount_kg_ha_y2",
                                                custom_colors = met.brewer(name="VanGogh3", n=7, type="discrete"),
                                                y_axis_label = "Nitrogen")
        add_significance_stars(base_plot, trait_statistical_analysis, "nitrogen")
      }
    ),

    # clipping
    tar_target(
      name = traits_clipping_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean_all |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "salinity"),
                                                grazing != "Natural"), 
                                                group_var = "grazing",
                                                custom_colors = met.brewer(name="Manet", n=3, type="discrete"),
                                                y_axis_label = "Clipping")
        add_significance_stars(base_plot, trait_statistical_analysis, "grazing")
      }
    ),

    # biomass
    tar_target(
      name = traits_biomass_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean_all |>
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "salinity"),
                                                grazing != "Natural") |>
                                                tidylog::left_join(standing_biomass_back |> 
                                                filter(year == 2022,
                                                grazing != "Natural") |>
                                                mutate(biomass_log = log(standing_biomass)) |>
                                                select(-year),
                                                by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")), 
                                                group_var = "biomass_log",
                                                custom_colors = met.brewer(name="VanGogh3", n=7, type="discrete"),
                                                y_axis_label = "Log(Standing biomass)")
        add_significance_stars(base_plot, trait_statistical_analysis, "biomass")
      }
    ),

  # Trait statistical analysis
  tar_target(
    name = trait_statistical_analysis,
    command = {
      test_treatment_effects(data = trait_mean_all, biomass_data = standing_biomass_back)
    }
  ),

  tar_target(
    name = trait_stats_table,
    command = make_trait_stats(trait_statistical_analysis)
  )

)
