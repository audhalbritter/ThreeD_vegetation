# trait analysis

trait_plan <- list(

  # impute traits for whole community (trait_fill)
  tar_target(
    name = trait_impute,
    command = make_trait_impute(cover_total,
                                 trait_raw,
                                 affinity)
  ),

  # bootstrap
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute) |>
        filter(trait_trans != "salinity")
  ),

  # make trait pca's
  #"mowing_frequency", 
  tar_target(
    name = affinity_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(trait_trans %in% c("plant_height_cm_log","temperature", "light", "moisture", "nutrients", "reaction", "grazing_pressure"))
    )
  ),

  tar_target(
    name = affinity_alpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "grazing_pressure"))
    )
  ),

    tar_target(
    name = affinity_subalpine_pca,
    command = make_trait_pca(trait_mean |> 
                            filter(grazing != "Natural") |>
                            filter(origSiteID == "Sub-alpine") |>
                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "grazing_pressure"))
    )
  ),

  tar_target(
    name = affinity_pca_plot,
    command = make_pca_plot_sites(trait_pca = affinity_pca, color_warm = warming_palette, biomass_div = biomass_div)
  ),

  # make trait pca plots
  tar_target(
    name = affinity_pca_plot_single,
    command = {
      # Calculate axis limits across both PCA datasets
      pc1_range <- range(affinity_alpine_pca[[1]]$PC1, affinity_alpine_pca[[2]]$PC1,
                        affinity_subalpine_pca[[1]]$PC1, affinity_subalpine_pca[[2]]$PC1)
      pc2_range <- range(affinity_alpine_pca[[1]]$PC2, affinity_alpine_pca[[2]]$PC2,
                        affinity_subalpine_pca[[1]]$PC2, affinity_subalpine_pca[[2]]$PC2)
      
      # Add some padding to the ranges
      pc1_limits <- c(pc1_range[1] - 0.1, pc1_range[2] + 0.1)
      pc2_limits <- c(pc2_range[1] - 0.1, pc2_range[2] + 0.1)
      
      # Create individual plots with tags and consistent axis limits
      p1 <- make_pca_plot(affinity_alpine_pca, title = "Affinity: alpine", color_warm = warming_palette) + 
        coord_equal(xlim = pc1_limits, ylim = pc2_limits) +
        labs(tag = "a)")
      p2 <- make_pca_plot(affinity_subalpine_pca, title = "Affinity: sub-alpine", color_warm = warming_palette) + 
        coord_equal(xlim = pc1_limits, ylim = pc2_limits) +
        labs(tag = "b)")
      
      # Combine with patchwork
      (p1 + p2) +
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
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "mowing_frequency", "grazing_pressure"),
                                                grazing != "Natural"), 
                                                group_var = "warming",
                                                custom_colors = warming_palette,
                                                y_axis_label = "",
                                                legend_name = "Warming",
                                                figure_names_order = c("Plant~height~(cm)",
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Mowing", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "warming")
      }
    ),

    tar_target(
      name = traits_nitrogen_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "mowing_frequency", "grazing_pressure"),
                                                grazing != "Natural") |>
                                                mutate(Namount_kg_ha_y2 = as.factor(Namount_kg_ha_y)), 
                                                group_var = "Namount_kg_ha_y2",
                                                custom_colors = nitrogen_palette,
                                                y_axis_label = expression(Nitrogen~addition~(kg~ha^-1~y^-1)),
                                                legend_name = "Nitrogen",
                                                figure_names_order = c("Plant~height~(cm)",
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Mowing", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "nitrogen")
      }
    ),

    # clipping
    tar_target(
      name = traits_clipping_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "mowing_frequency", "grazing_pressure"),
                                                grazing != "Natural"), 
                                                group_var = "grazing",
                                                custom_colors = grazing_palette,
                                                y_axis_label = "Clipping",
                                                legend_name = "Clipping",
                                                figure_names_order = c("Plant~height~(cm)",
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Mowing", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "grazing")
      }
    ),

    # biomass
    tar_target(
      name = traits_biomass_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |>
                                                filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "mowing_frequency", "grazing_pressure"),
                                                grazing != "Natural") |>
                                                tidylog::left_join(standing_biomass_back |> 
                                                filter(year == 2022,
                                                grazing != "Natural") |>
                                                mutate(biomass_log = log(standing_biomass)) |>
                                                select(-year),
                                                by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")), 
                                                group_var = "biomass_log",
                                                custom_colors = met.brewer(name="Hokusai2", n=5, type="discrete"),
                                                y_axis_label = "Log(Standing biomass)",
                                                legend_name = "Biomass",
                                                figure_names_order = c("Plant~height~(cm)",
                                               "Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Mowing", "Grazing"))
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
  ),

  # # CHECK TRAIT IMPUTATION COVERAGE
  # # trait coverage plot (check how much of the community has been sampled)
  # tar_target(
  #   name = trait_coverage,
  #   command = fortify(trait_impute) |> 
  #     ungroup() |>
  #     complete(.id, level, trait_trans, fill = list(s = 0)) |>
  #     filter(level == "turfID") |>
  #     group_by(origSiteID, treatment_comm, trait_trans) |>
  #     # prob = 0.25 gives 75% of the plots
  #     # also run prob = 0.5 for 50% of the plots
  #     summarise(q = quantile(s, prob = 0.25))
  
  # ),

  # # trait imputation plot

  # tar_target(
  #   name = trait_names,
  #   command = c(
  #     "plant_height_log_cm" = "Height cm",
  #     "dry_mass_g_log" = "Dry mass g",
  #     "leaf_area_log_cm2" = "Area cm2",
  #     "leaf_thickness_log_mm" = "Thickness mm",
  #     "ldmc" = "LDMC",
  #     "sla_cm2_g" = "SLA cm2/g")
  # ),

  tar_target(
    name = imputation_plot,
    command = {

        trait_names <- c(
      "plant_height_log_cm" = "Height cm",
      "dry_mass_g_log" = "Dry mass g",
      "leaf_area_cm2_log" = "Area cm2",
      "leaf_thickness_mm_log" = "Thickness mm",
      "ldmc" = "LDMC",
      "sla_cm2_g" = "SLA cm2/g",
      "temperature" = "Temperature",
      "light" = "Light",
      "moisture" = "Moisture",
      "nutrients" = "Nutrients",
      "reaction" = "Reaction",
      "salinity" = "Salinity",
      "mowing_frequency" = "Mowing",
      "grazing_pressure" = "Grazing")

      dd <- trait_impute %>% 
        ungroup() |> 
        distinct(origSiteID, origBlockID, turfID, warming, grazing, Namount_kg_ha_y) |> 
        arrange(origSiteID, origBlockID, turfID) |> 
        mutate(ID = paste0(origSiteID, "_ ", origBlockID, "_", turfID),
               ID2 = paste0(origSiteID, "-", substr(warming, 1,1), "-", substr(grazing, 1,1))) |> 
        select(ID, ID2)

      # print list of treatments
      #cat(paste0('c("', paste(dd$ID2, collapse = '", "'), '")'))

      #check trait coverage
      trait_impute %>% 
        autoplot(., other_col_how = "ignore") +
        scale_y_continuous(breaks = c(0, 0.5, 1)) +

        facet_wrap(~ trait_trans, labeller = labeller(trait_trans = trait_names)) +
        labs(x = "Treatments") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90))

    }
  )

)
