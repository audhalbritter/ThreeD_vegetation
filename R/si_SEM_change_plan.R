# si SEM plan

si_SEM_change_plan <- list(

# CHANGE IN BIOMASS AND DIVERSITY (LOG RATIO)
  # CLIPPING
  
  # prep diversity data
  tar_target(
    name = cut_change_all_diversity,
    command = {
    dat1_div <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)

    # alpine
    mod1 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")

    # sub-alpine
    mod2 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out2 <- summary(mod2)

      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")


      # change in richness
      dat2 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_richness,
                            biomass = log_ratio_bio)

      # change in richness alpine
      mod3 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")


    # change in evenness
      dat3 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_evenness,
                            biomass = log_ratio_bio)

      # change in evenness alpine
      mod5 <- run_SEM(data = dat3 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out5 <- summary(mod5)

      fig5 <- make_SEM_figure(sem_results = out5,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # change in evenness sub-alpine
      mod6 <- run_SEM(data = dat3 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out6 <- summary(mod6)

      fig6 <- make_SEM_figure(sem_results = out6,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")


         # Combine all 6 panels (diversity, richness, evenness × Alpine, Sub-alpine)
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


  # GRAZING
  tar_target(
    name = graz_change_all_diversity,
    command = {
      # Diversity analysis
      dat1_div <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = log_ratio_diversity,
                                biomass = log_ratio_bio)

      # Diversity alpine
      mod1 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out1 <- summary(mod1)
      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # Diversity sub-alpine
      mod2 <- run_SEM(data = dat1_div |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out2 <- summary(mod2)
      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # Richness analysis
      dat2_rich <- prep_SEM_data(data = biomass_div,
                                 landuse = "grazing",
                                 diversity = log_ratio_richness,
                                 biomass = log_ratio_bio)

      # Richness alpine
      mod3 <- run_SEM(data = dat2_rich |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out3 <- summary(mod3)
      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Richness sub-alpine
      mod4 <- run_SEM(data = dat2_rich |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out4 <- summary(mod4)
      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # Evenness analysis
      dat3_even <- prep_SEM_data(data = biomass_div,
                                 landuse = "grazing",
                                 diversity = log_ratio_evenness,
                                 biomass = log_ratio_bio)

      # Evenness alpine
      mod5 <- run_SEM(data = dat3_even |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
      out5 <- summary(mod5)
      fig5 <- make_SEM_figure(sem_results = out5,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Evenness sub-alpine
      mod6 <- run_SEM(data = dat3_even |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
      out6 <- summary(mod6)
      fig6 <- make_SEM_figure(sem_results = out6,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # Combine all 6 panels (diversity, richness, evenness × Alpine, Sub-alpine)
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
