# si SEM plan

si_SEM_final_plan <- list(

  # Combined richness and evenness figure with 4 panels
  tar_target(
    name = cut_final_richness_evenness,
    command = {

      # Richness analysis
      dat1_rich <- prep_SEM_data(data = biomass_div,
                                 landuse = "clipping",
                                 diversity = final_richness,
                                 biomass = final_bio)

      # Richness alpine
      mod1 <- run_SEM(data = dat1_rich |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")
      out1 <- summary(mod1)
      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Richness sub-alpine
      mod2 <- run_SEM(data = dat1_rich |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")
      out2 <- summary(mod2)
      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Evenness analysis
      dat1_even <- prep_SEM_data(data = biomass_div,
                                 landuse = "clipping",
                                 diversity = final_evenness,
                                 biomass = final_bio)

      # Evenness alpine
      mod3 <- run_SEM(data = dat1_even |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")
      out3 <- summary(mod3)
      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Evenness sub-alpine
      mod4 <- run_SEM(data = dat1_even |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")
      out4 <- summary(mod4)
      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Combine all 4 panels
      figure <- (fig1 + fig2) / (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine richness', 'b) Sub-alpine richness', 
                                           'c) Alpine evenness', 'd) Sub-alpine evenness'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      # Combine all results
      out <- bind_rows(
        "Alpine richness" = out1$coefficients,
        "Sub-alpine richness" = out2$coefficients,
        "Alpine evenness" = out3$coefficients,
        "Sub-alpine evenness" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),



  # grazing and diversity
  tar_target(
    name = graz_final_diversity,
    command = {

      # final diversity
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_diversity,
                            biomass = final_bio)

      # final diversity alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # final diversity sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  # Combined grazing figure with 6 panels (diversity, richness, evenness × Alpine, Sub-alpine)
  tar_target(
    name = graz_final_all_div,
    command = {

      # Diversity analysis
      dat1_div <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = final_diversity,
                                biomass = final_bio)

      # Diversity alpine
      mod1 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out1 <- summary(mod1)
      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # Diversity sub-alpine
      mod2 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out2 <- summary(mod2)
      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # Richness analysis
      dat1_rich <- prep_SEM_data(data = biomass_div,
                                 landuse = "grazing",
                                 diversity = final_richness,
                                 biomass = final_bio)

      # Richness alpine
      mod3 <- run_SEM(data = dat1_rich |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out3 <- summary(mod3)
      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Richness sub-alpine
      mod4 <- run_SEM(data = dat1_rich |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out4 <- summary(mod4)
      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Evenness analysis
      dat1_even <- prep_SEM_data(data = biomass_div,
                                 landuse = "grazing",
                                 diversity = final_evenness,
                                 biomass = final_bio)

      # Evenness alpine
      mod5 <- run_SEM(data = dat1_even |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out5 <- summary(mod5)
      fig5 <- make_SEM_figure(sem_results = out5,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Evenness sub-alpine
      mod6 <- run_SEM(data = dat1_even |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out6 <- summary(mod6)
      fig6 <- make_SEM_figure(sem_results = out6,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Combine all 6 panels in 2 rows × 3 columns layout
      figure <- (fig1 + fig2) / (fig3 + fig4) / (fig5 + fig6) +
        plot_annotation(tag_levels = list(c('a) Alpine diversity', 'b) Sub-alpine diversity', 'c) Alpine richness',
                                           'd) Sub-alpine richness', 'e) Alpine evenness', 'f) Sub-alpine evenness'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      # Combine all results
      out <- bind_rows(
        "Alpine diversity" = out1$coefficients,
        "Sub-alpine diversity" = out2$coefficients,
        "Alpine richness" = out3$coefficients,
        "Sub-alpine richness" = out4$coefficients,
        "Alpine evenness" = out5$coefficients,
        "Sub-alpine evenness" = out6$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  # Clipping and environmental affinities (temperature, moisture, nutrients, grazing pressure)
  tar_target(
    name = cut_final_affinity_all,
    command = {

      # Temperature affinity data
      dat_temp <- biomass_div |>
        tidylog::left_join(trait_mean |>
                             filter(trait_trans == "temperature"),
                           by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log)) |>
        filter(grazing != "Natural")

      dat_temp1 <- prep_SEM_data(data = dat_temp,
                                 landuse = "clipping",
                                 diversity = mean,
                                 biomass = final_bio)

      # Temperature affinity alpine
      mod_temp_alpine <- run_SEM(data = dat_temp1 |>
                                   filter(origSiteID == "Alpine"),
                                 landuse = "clipping")
      out_temp_alpine <- summary(mod_temp_alpine)
      fig_temp_alpine <- make_SEM_figure(sem_results = out_temp_alpine,
                                         type = "final",
                                         landuse = "clipping",
                                         col = treatment_palette,
                                         diversity_type = "temperature affinity")

      # Temperature affinity sub-alpine
      mod_temp_sub <- run_SEM(data = dat_temp1 |>
                                filter(origSiteID == "Sub-alpine"),
                              landuse = "clipping")
      out_temp_sub <- summary(mod_temp_sub)
      fig_temp_sub <- make_SEM_figure(sem_results = out_temp_sub,
                                      type = "final",
                                      landuse = "clipping",
                                      col = treatment_palette,
                                      diversity_type = "temperature affinity")

      # Moisture affinity data
      dat_moist <- biomass_div |>
        tidylog::left_join(trait_mean |>
                             filter(trait_trans == "moisture"),
                           by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log)) |>
        filter(grazing != "Natural")

      dat_moist1 <- prep_SEM_data(data = dat_moist,
                                  landuse = "clipping",
                                  diversity = mean,
                                  biomass = final_bio)

      # Moisture affinity alpine
      mod_moist_alpine <- run_SEM(data = dat_moist1 |>
                                    filter(origSiteID == "Alpine"),
                                  landuse = "clipping")
      out_moist_alpine <- summary(mod_moist_alpine)
      fig_moist_alpine <- make_SEM_figure(sem_results = out_moist_alpine,
                                          type = "final",
                                          landuse = "clipping",
                                          col = treatment_palette,
                                          diversity_type = "moisture affinity")

      # Moisture affinity sub-alpine
      mod_moist_sub <- run_SEM(data = dat_moist1 |>
                                 filter(origSiteID == "Sub-alpine"),
                               landuse = "clipping")
      out_moist_sub <- summary(mod_moist_sub)
      fig_moist_sub <- make_SEM_figure(sem_results = out_moist_sub,
                                       type = "final",
                                       landuse = "clipping",
                                       col = treatment_palette,
                                       diversity_type = "moisture affinity")

      # Nutrient affinity data
      dat_nutr <- biomass_div |>
        tidylog::left_join(trait_mean |>
                             filter(trait_trans == "nutrients"),
                           by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log)) |>
        filter(grazing != "Natural")

      dat_nutr1 <- prep_SEM_data(data = dat_nutr,
                                 landuse = "clipping",
                                 diversity = mean,
                                 biomass = final_bio)

      # Nutrient affinity alpine
      mod_nutr_alpine <- run_SEM(data = dat_nutr1 |>
                                   filter(origSiteID == "Alpine"),
                                 landuse = "clipping")
      out_nutr_alpine <- summary(mod_nutr_alpine)
      fig_nutr_alpine <- make_SEM_figure(sem_results = out_nutr_alpine,
                                         type = "final",
                                         landuse = "clipping",
                                         col = treatment_palette,
                                         diversity_type = "nutrient affinity")

      # Nutrient affinity sub-alpine
      mod_nutr_sub <- run_SEM(data = dat_nutr1 |>
                                filter(origSiteID == "Sub-alpine"),
                              landuse = "clipping")
      out_nutr_sub <- summary(mod_nutr_sub)
      fig_nutr_sub <- make_SEM_figure(sem_results = out_nutr_sub,
                                      type = "final",
                                      landuse = "clipping",
                                      col = treatment_palette,
                                      diversity_type = "nutrient affinity")

      # Grazing pressure affinity data
      dat_graz <- biomass_div |>
        tidylog::left_join(trait_mean |>
                             filter(trait_trans == "grazing_pressure"),
                           by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log)) |>
        filter(grazing != "Natural")

      dat_graz1 <- prep_SEM_data(data = dat_graz,
                                 landuse = "clipping",
                                 diversity = mean,
                                 biomass = final_bio)

      # Grazing pressure affinity alpine
      mod_graz_alpine <- run_SEM(data = dat_graz1 |>
                                   filter(origSiteID == "Alpine"),
                                 landuse = "clipping")
      out_graz_alpine <- summary(mod_graz_alpine)
      fig_graz_alpine <- make_SEM_figure(sem_results = out_graz_alpine,
                                         type = "final",
                                         landuse = "clipping",
                                         col = treatment_palette,
                                         diversity_type = "grazing pressure affinity")

      # Grazing pressure affinity sub-alpine
      mod_graz_sub <- run_SEM(data = dat_graz1 |>
                                filter(origSiteID == "Sub-alpine"),
                              landuse = "clipping")
      out_graz_sub <- summary(mod_graz_sub)
      fig_graz_sub <- make_SEM_figure(sem_results = out_graz_sub,
                                      type = "final",
                                      landuse = "clipping",
                                      col = treatment_palette,
                                      diversity_type = "grazing pressure affinity")

      # Combine all 8 panels: 4 affinities × 2 sites
      figure <- (fig_temp_alpine + fig_temp_sub) /
        (fig_moist_alpine + fig_moist_sub) /
        (fig_nutr_alpine + fig_nutr_sub) /
        (fig_graz_alpine + fig_graz_sub) +
        plot_annotation(tag_levels = list(c(
          'a) Alpine temperature', 'b) Sub-alpine temperature',
          'c) Alpine moisture', 'd) Sub-alpine moisture',
          'e) Alpine nutrients', 'f) Sub-alpine nutrients',
          'g) Alpine grazing pressure', 'h) Sub-alpine grazing pressure'
        ))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      # Combine all results
      out <- bind_rows(
        "Temperature affinity: Alpine" = out_temp_alpine$coefficients,
        "Temperature affinity: Sub-alpine" = out_temp_sub$coefficients,
        "Moisture affinity: Alpine" = out_moist_alpine$coefficients,
        "Moisture affinity: Sub-alpine" = out_moist_sub$coefficients,
        "Nutrient affinity: Alpine" = out_nutr_alpine$coefficients,
        "Nutrient affinity: Sub-alpine" = out_nutr_sub$coefficients,
        "Grazing pressure affinity: Alpine" = out_graz_alpine$coefficients,
        "Grazing pressure affinity: Sub-alpine" = out_graz_sub$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
