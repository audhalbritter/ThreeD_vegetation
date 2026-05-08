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

    # Trats ridgeline plot
    # warming
    tar_target(
      name = traits_warming_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("temperature", "light", "moisture", "nutrients", "reaction", "grazing_pressure"),
                                                grazing != "Natural"), 
                                                group_var = "warming",
                                                custom_colors = warming_palette,
                                                y_axis_label = "",
                                                legend_name = "Warming",
                                                figure_names_order = c("Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "warming")
      }
    ),

    tar_target(
      name = traits_nitrogen_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("temperature", "light", "moisture", "nutrients", "reaction", "grazing_pressure"),
                                                grazing != "Natural") |>
                                                mutate(Namount_kg_ha_y2 = as.factor(Namount_kg_ha_y)), 
                                                group_var = "Namount_kg_ha_y2",
                                                custom_colors = nitrogen_palette,
                                                y_axis_label = expression(Nitrogen~addition~(kg~ha^-1~y^-1)),
                                                legend_name = "Nitrogen",
                                                figure_names_order = c("Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "nitrogen")
      }
    ),

    # clipping
    tar_target(
      name = traits_clipping_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |> 
                                                filter(trait_trans %in% c("temperature",  "light", "moisture", "nutrients", "reaction", "grazing_pressure"),
                                                grazing != "Natural"), 
                                                group_var = "grazing",
                                                custom_colors = grazing_palette,
                                                y_axis_label = "Clipping",
                                                legend_name = "Clipping",
                                                figure_names_order = c("Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Grazing"))
        add_significance_stars(base_plot, trait_statistical_analysis, "grazing")
      }
    ),

    # biomass
    tar_target(
      name = traits_biomass_plot,
      command = {
        base_plot <- make_trait_ridgeline_plot(trait_mean |>
                                                filter(trait_trans %in% c("temperature",  "light", "moisture", "nutrients", "reaction", "grazing_pressure"),
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
                                                figure_names_order = c("Light", "Temperature", "Nutrients", 
                                               "Reaction", "Moisture", "Grazing"))
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

  # Proportion of total cover accounted for by species with Ellenberg indicator values,
  # per plot and indicator, then summarised as mean and range across plots
  tar_target(
    name = ellenberg_coverage,
    command = {
      ellenberg_indicators <- c("light", "temperature", "moisture", "nutrients",
                                "reaction", "grazing_pressure")

      trait_impute |>
        ungroup() |>
        filter(trait_trans %in% ellenberg_indicators) |>
        # Collapse to one row per species x plot x indicator
        # (trait_impute has multiple rows per species x plot x trait from bootstrapping)
        distinct(turfID, origSiteID, species, trait_trans, cover, sum_abun) |>
        # Sum cover of all species that have each indicator per plot
        group_by(turfID, origSiteID, trait_trans) |>
        summarise(
          cover_with_indicator = sum(cover),
          sum_abun = first(sum_abun),
          .groups = "drop"
        ) |>
        mutate(prop_cover = cover_with_indicator / sum_abun) |>
        # Summarise across plots
        summarise(
          mean_prop = mean(prop_cover),
          se_prop   = sd(prop_cover) / sqrt(n())
        )
    }
  ),

  # # trait imputation plot

  tar_target(
    name = imputation_plot,
    command = {

        trait_names <- c(
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
