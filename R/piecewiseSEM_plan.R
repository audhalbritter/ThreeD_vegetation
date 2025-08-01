# piecewiseSEM plan

piecewiseSEM_plan <- list(

  ### colours

  # Final biomass and diversity indices
  # clipping and diversity
  tar_target(
    name = cut_final_diversity,
    command = {

      # final diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = final_diversity,
                            biomass = final_bio)

      # final diversity alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette)

      # final diversity sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette)

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
  )

)
