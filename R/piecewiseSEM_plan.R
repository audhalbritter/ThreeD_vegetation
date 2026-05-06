# piecewiseSEM plan (clipping × final diversity; figures via ggraph in [make_SEM_figure])

piecewiseSEM_plan <- list(

  tar_target(
    name = cut_final_diversity,
    command = {

      dat1 <- prep_SEM_data(
        data = biomass_div,
        landuse = "clipping",
        diversity = final_diversity,
        biomass = final_bio
      )

      mod3 <- run_SEM(
        data = dat1 |> filter(origSiteID == "Alpine"),
        landuse = "clipping"
      )

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(
        sem_results = out3,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "diversity"
      )

      mod4 <- run_SEM(
        data = dat1 |> filter(origSiteID == "Sub-alpine"),
        landuse = "clipping"
      )

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(
        sem_results = out4,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "diversity"
      )

      sem_tag_theme <- ggplot2::theme(
        plot.tag.location = "margin",
        plot.tag.position = "topleft",
        plot.tag = ggplot2::element_text(size = 12, hjust = 0, vjust = 1),
        plot.margin = ggplot2::margin(22, 14, 10, 14)
      )

      fig3 <- fig3 +
        ggplot2::labs(tag = "a) Alpine") +
        sem_tag_theme
      fig4 <- fig4 +
        ggplot2::labs(tag = "b) Sub-alpine") +
        sem_tag_theme

      figure <- fig3 + fig4 + patchwork::plot_layout(ncol = 2)

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
