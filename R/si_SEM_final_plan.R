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
  )

)
