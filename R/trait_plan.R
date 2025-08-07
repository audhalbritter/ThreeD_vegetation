# trait analysis

trait_plan <- list(

  # impute traits for whole community (trait_fill)
  tar_target(
    name = trait_impute,
    command = make_trait_impute(cover_total,
                                 trait_raw,
                                 ellenberg)
  ),

  # bootstrap
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute) |>
        filter(trait_trans != "salinity")
  ),

  # make trait pca's
  tar_target(
    name = affinity_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction"))
    )
  ),

  tar_target(
    name = affinity_alpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction"))
    )
  ),

    tar_target(
    name = affinity_subalpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Sub-alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction"))
    )
  ),

  tar_target(
    name = trait_alpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "leaf_thickness_mm_log", "sla_cm2_g", "ldmc"))
    )
  ),

  tar_target(
    name = trait_subalpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Sub-alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "leaf_thickness_mm_log", "sla_cm2_g", "ldmc"))
    )
  ),

  tar_target(
    name = affinity_pca_plot,
    command = make_pca_plot_sites(affinity_pca, title = "Affinity", color_warm = warming_palette)
  ),

  # make trait pca plots
  tar_target(
    name = trait_pca_plot,
    command = {
      # Create individual plots with tags
              p1 <- make_pca_plot(affinity_alpine_pca, title = "Affinity: alpine", color_warm = warming_palette) + 
        labs(tag = "a)")
              p2 <- make_pca_plot(affinity_subalpine_pca, title = "Affinity: sub-alpine", color_warm = warming_palette) + 
        labs(tag = "b)")
              p3 <- make_pca_plot(trait_alpine_pca, title = "Traits: alpine", color_warm = warming_palette) + 
        labs(tag = "c)")
              p4 <- make_pca_plot(trait_subalpine_pca, title = "Traits: sub-alpine", color_warm = warming_palette) + 
        labs(tag = "d)")
      
      # Combine with patchwork
      (p1 + p2) / (p3 + p4) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
    }
  ),

    # Trats ridgeline plot
    # warming
    tar_target(
      name = traits_warming_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction"),
                                                grazing != "Natural"), 
                                                group_var = "warming",
                                                custom_colors = warming_palette,
                                                y_axis_label = "",
                                                figure_names_order = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", 
                                               "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", 
                                               "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})", 
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture"))
        add_significance_stars(base_plot, trait_statistical_analysis, "warming")
      }
    ),

    tar_target(
      name = traits_nitrogen_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction"),
                                                grazing != "Natural") |>
                                                mutate(Namount_kg_ha_y2 = as.factor(Namount_kg_ha_y)), 
                                                group_var = "Namount_kg_ha_y2",
                                                custom_colors = met.brewer(name="VanGogh3", n=7, type="discrete"),
                                                y_axis_label = "Nitrogen",
                                                figure_names_order = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", 
                                               "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", 
                                               "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})", 
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture"))
        add_significance_stars(base_plot, trait_statistical_analysis, "nitrogen")
      }
    ),

    # clipping
    tar_target(
      name = traits_clipping_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction"),
                                                grazing != "Natural"), 
                                                group_var = "grazing",
                                                custom_colors = grazing_palette,
                                                y_axis_label = "Clipping",
                                                figure_names_order = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", 
                                               "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", 
                                               "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})", 
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture"))
        add_significance_stars(base_plot, trait_statistical_analysis, "grazing")
      }
    ),

    # biomass
    tar_target(
      name = traits_biomass_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |>
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction"),
                                                grazing != "Natural") |>
                                                tidylog::left_join(standing_biomass_back |> 
                                                filter(year == 2022,
                                                grazing != "Natural") |>
                                                mutate(biomass_log = log(standing_biomass)) |>
                                                select(-year),
                                                by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")), 
                                                group_var = "biomass_log",
                                                custom_colors = biomass_palette,
                                                y_axis_label = "Log(Standing biomass)",
                                                figure_names_order = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", 
                                               "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", 
                                               "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})", 
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture"))
        add_significance_stars(base_plot, trait_statistical_analysis, "biomass")
      }
    ),

  # Trait statistical analysis
  tar_target(
    name = trait_statistical_analysis,
    command = {
      test_treatment_effects(data = trait_mean, biomass_data = standing_biomass_back)
    }
  ),

  tar_target(
    name = trait_stats_table,
    command = make_trait_stats(trait_statistical_analysis)
  )

)
