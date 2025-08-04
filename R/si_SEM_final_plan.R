# si SEM plan

si_SEM_final_plan <- list(

  tar_target(
    name = cut_final_richness,
    command = {

      # final richness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = final_richness,
                            biomass = final_bio)

      # final richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      # final richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

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

  # clipping and evenness
  tar_target(
    name = cut_final_evenness,
    command = {

      # final evenness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = final_evenness,
                            biomass = final_bio)

      # final evenness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # final evenness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

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

  # clipping and richness
  tar_target(
    name = graz_final_richness,
    command = {

      # final richness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_richness,
                            biomass = final_bio)

      # final richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # final richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

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

  # clipping and evenness
  tar_target(
    name = graz_final_evenness,
    command = {

      # final evenness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_evenness,
                            biomass = final_bio)

      # final evenness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # final evenness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

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
  )

)
