# si SEM plan (figures use ggraph pipeline in [make_SEM_figure])


si_SEM_final_plan <- list(

  tar_target(
    name = cut_final_richness_evenness,
    command = {

      sem_tag_theme <- ggplot2::theme(
        plot.tag.location = "margin",
        plot.tag.position = "topleft",
        plot.tag = ggplot2::element_text(size = 12, hjust = 0, vjust = 1),
        plot.margin = ggplot2::margin(22, 14, 10, 14)
      )

      dat1_rich <- prep_SEM_data(
        data = biomass_div,
        landuse = "clipping",
        diversity = final_richness,
        biomass = final_bio
      )

      mod1 <- run_SEM(
        data = dat1_rich |> filter(origSiteID == "Alpine"),
        landuse = "clipping"
      )
      out1 <- summary(mod1)
      fig1 <- make_SEM_figure(
        sem_results = out1,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "richness"
      ) +
        ggplot2::labs(tag = "a) Alpine richness") +
        sem_tag_theme

      mod2 <- run_SEM(
        data = dat1_rich |> filter(origSiteID == "Sub-alpine"),
        landuse = "clipping"
      )
      out2 <- summary(mod2)
      fig2 <- make_SEM_figure(
        sem_results = out2,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "richness"
      ) +
        ggplot2::labs(tag = "b) Sub-alpine richness") +
        sem_tag_theme

      dat1_even <- prep_SEM_data(
        data = biomass_div,
        landuse = "clipping",
        diversity = final_evenness,
        biomass = final_bio
      )

      mod3 <- run_SEM(
        data = dat1_even |> filter(origSiteID == "Alpine"),
        landuse = "clipping"
      )
      out3 <- summary(mod3)
      fig3 <- make_SEM_figure(
        sem_results = out3,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "evenness"
      ) +
        ggplot2::labs(tag = "c) Alpine evenness") +
        sem_tag_theme

      mod4 <- run_SEM(
        data = dat1_even |> filter(origSiteID == "Sub-alpine"),
        landuse = "clipping"
      )
      out4 <- summary(mod4)
      fig4 <- make_SEM_figure(
        sem_results = out4,
        landuse = "clipping",
        col = treatment_palette,
        diversity_type = "evenness"
      ) +
        ggplot2::labs(tag = "d) Sub-alpine evenness") +
        sem_tag_theme

      # 2×2 grid: nested `ncol` was squeezing coord_fixed SEM panels; use `/` rows + `|`
      figure <- (fig1 | fig2) / (fig3 | fig4) +
        patchwork::plot_layout(heights = c(1, 1), widths = c(1, 1))

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

  tar_target(
    name = graz_final_diversity,
    command = {

      sem_tag_theme <- ggplot2::theme(
        plot.tag.location = "margin",
        plot.tag.position = "topleft",
        plot.tag = ggplot2::element_text(size = 12, hjust = 0, vjust = 1),
        plot.margin = ggplot2::margin(22, 14, 10, 14)
      )

      dat1 <- prep_SEM_data(
        data = biomass_div,
        landuse = "grazing",
        diversity = final_diversity,
        biomass = final_bio
      )

      mod3 <- run_SEM(
        data = dat1 |> filter(origSiteID == "Alpine"),
        landuse = "grazing"
      )

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(
        sem_results = out3,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "diversity"
      ) +
        ggplot2::labs(tag = "a) Alpine") +
        sem_tag_theme

      mod4 <- run_SEM(
        data = dat1 |> filter(origSiteID == "Sub-alpine"),
        landuse = "grazing"
      )

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(
        sem_results = out4,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "diversity"
      ) +
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
  ),

  tar_target(
    name = graz_final_all_div,
    command = {

      sem_tag_theme <- ggplot2::theme(
        plot.tag.location = "margin",
        plot.tag.position = "topleft",
        plot.tag = ggplot2::element_text(size = 12, hjust = 0, vjust = 1),
        plot.margin = ggplot2::margin(22, 14, 10, 14)
      )

      dat1_div <- prep_SEM_data(
        data = biomass_div,
        landuse = "grazing",
        diversity = final_diversity,
        biomass = final_bio
      )

      mod1 <- run_SEM(
        data = dat1_div |> filter(origSiteID == "Alpine"),
        landuse = "grazing"
      )
      out1 <- summary(mod1)
      fig1 <- make_SEM_figure(
        sem_results = out1,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "diversity"
      ) +
        ggplot2::labs(tag = "a) Alpine diversity") +
        sem_tag_theme

      mod2 <- run_SEM(
        data = dat1_div |> filter(origSiteID == "Sub-alpine"),
        landuse = "grazing"
      )
      out2 <- summary(mod2)
      fig2 <- make_SEM_figure(
        sem_results = out2,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "diversity"
      ) +
        ggplot2::labs(tag = "b) Sub-alpine diversity") +
        sem_tag_theme

      dat1_rich <- prep_SEM_data(
        data = biomass_div,
        landuse = "grazing",
        diversity = final_richness,
        biomass = final_bio
      )

      mod3 <- run_SEM(
        data = dat1_rich |> filter(origSiteID == "Alpine"),
        landuse = "grazing"
      )
      out3 <- summary(mod3)
      fig3 <- make_SEM_figure(
        sem_results = out3,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "richness"
      ) +
        ggplot2::labs(tag = "c) Alpine richness") +
        sem_tag_theme

      mod4 <- run_SEM(
        data = dat1_rich |> filter(origSiteID == "Sub-alpine"),
        landuse = "grazing"
      )
      out4 <- summary(mod4)
      fig4 <- make_SEM_figure(
        sem_results = out4,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "richness"
      ) +
        ggplot2::labs(tag = "d) Sub-alpine richness") +
        sem_tag_theme

      dat1_even <- prep_SEM_data(
        data = biomass_div,
        landuse = "grazing",
        diversity = final_evenness,
        biomass = final_bio
      )

      mod5 <- run_SEM(
        data = dat1_even |> filter(origSiteID == "Alpine"),
        landuse = "grazing"
      )
      out5 <- summary(mod5)
      fig5 <- make_SEM_figure(
        sem_results = out5,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "evenness"
      ) +
        ggplot2::labs(tag = "e) Alpine evenness") +
        sem_tag_theme

      mod6 <- run_SEM(
        data = dat1_even |> filter(origSiteID == "Sub-alpine"),
        landuse = "grazing"
      )
      out6 <- summary(mod6)
      fig6 <- make_SEM_figure(
        sem_results = out6,
        landuse = "grazing",
        col = treatment_palette,
        diversity_type = "evenness"
      ) +
        ggplot2::labs(tag = "f) Sub-alpine evenness") +
        sem_tag_theme

      figure <- (fig1 | fig2) / (fig3 | fig4) / (fig5 | fig6) +
        patchwork::plot_layout(heights = c(1, 1, 1), widths = c(1, 1))

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
