# piecewiseSEM plan

piecewiseSEM_plan <- list(

  ### colours

  tar_target(
    name = sem_colour,
    command = wes_palette("FrenchDispatch")
  ),

  # Final biomass and diversity indices
  # cutting and diversity
  tar_target(
    name = cut_final_diversity,
    command = {

      # final diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = final_diversity,
                            biomass = final_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "cutting")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "cutting")

      out2 <- summary(mod2)

      fig2 <- make_SEM_site_figure(sem_results = out2,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # final diversity alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # final diversity sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        #Across = out1$coefficients,
        #Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  # Run SEM separate by warming treatment
  # cutting and diversity
  tar_target(
    name = cut_final_diversity_AW,
    command = {

      # final diversity
      dat1 <- prep_SEM_data(data = biomass_div,
        landuse = "cutting",
        diversity = final_diversity,
        biomass = final_bio)

      # final diversity alpine - ambient
      mod1 <- run_AW_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine",
                               warming == 0),
                      landuse = "cutting")

      out1 <- summary(mod1)

      fig1 <- make_SEM_AW_figure(sem_results = out1,
                                type = "final",
                                landuse = "cutting",
                                col = sem_colour)
      
      # final diversity alpine - warming
      mod2 <- run_AW_SEM(data = dat1 |>
        filter(origSiteID == "Alpine",
               warming == 1),
      landuse = "cutting")
      
      out2 <- summary(mod2)
      
      fig2 <- make_SEM_AW_figure(sem_results = out2,
                type = "final",
                landuse = "cutting",
                col = sem_colour)
      
      # final diversity sub-alpine - ambient
      mod3 <- run_AW_SEM(data = dat1 |>
        filter(origSiteID == "Sub-alpine",
      warming == 0),
      landuse = "cutting")

      out3 <- summary(mod3)

      fig3 <- make_SEM_AW_figure(sem_results = out3,
              type = "final",
              landuse = "cutting",
              col = sem_colour)

      # final diversity sub-alpine - warming
      mod4 <- run_AW_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine",
                      warming == 1),
                      landuse = "cutting")

      out4 <- summary(mod4)

      fig4 <- make_SEM_AW_figure(sem_results = out4,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      figure <- (fig1 + fig2) /
        (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine - ambient', 'b) Alpine - warm', 'c) Sub-alpine ambient', 'd) Sub-alpine warm'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        #Across = out1$coefficients,
        #Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
