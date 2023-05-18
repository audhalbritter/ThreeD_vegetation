# Figures for Nitrogen_log
figure_plan2 <- list(

  ### PRODUCTIVTY

  # productivity and cover figure in one
  tar_target(
    name = prod_cover_figure2,
    command = {

      productivity_text2 <- productivity_stats2 |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        ### BY HAND CODE!!!
        filter(significance == "sign") |>
        filter(origSiteID == "Alpine" | origSiteID == "Sub-alpine" & term == "GxN") |>
        mutate(term = if_else(term == "GxN", "W + N + GxN", term)) |>
        distinct(origSiteID, term)

      productivity_figure2 <- make_vegetation_figure(dat = productivity_prediction2,
                                                    x_axis = Nitrogen_log,
                                                    yaxislabel = bquote(Annual~productivity~g~m^-2~y^-1),
                                                    colourpalette = col_palette,
                                                    linetypepalette = c("solid", "dashed", "dotted"),
                                                    shapepalette = c(16, 0, 2),
                                                    facet_2 = NA) +
        labs(tag = "a)") +
        # add stats
        geom_text(data = productivity_prediction2 |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text2, by = "origSiteID"),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)


      cover_text2 <- cover_stats2 |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, functional_group, term) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))


      cover_figure2 <- make_vegetation_figure(dat = cover_prediction2,
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group") +
        labs(tag = "b)") +
        # add stats
        geom_text(data = cover_prediction2 |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text2, by = c("origSiteID", "functional_group")) |>
                    mutate(term = if_else(is.na(term), "", term)),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)

      productivity_figure2 + cover_figure2 +
        plot_layout(guides = "collect", widths = c(1, 2)) &
        theme(legend.position = "bottom") &
        guides(linetype = guide_legend(nrow = 2, byrow = TRUE),
               shape = guide_legend(nrow = 2, byrow = TRUE),
               colour = guide_legend(nrow = 2, byrow = TRUE))

    }
  ),

  # # text
  # tar_target(
  #   name = productivity_text2,
  #   command = productivity_stats2 |>
  #     mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
  #                                     term == "Intercept" ~ "non-sign",
  #                                     TRUE ~ "sign")) |>
  #     ### BY HAND CODE!!!
  #     filter(significance == "sign") |>
  #     filter(origSiteID == "Alpine" | origSiteID == "Sub-alpine" & term == "GxN") |>
  #     mutate(term = if_else(term == "GxN", "W + N + GxN", term)) |>
  #     distinct(origSiteID, term)
  # ),
  #
  # # figure
  # tar_target(
  #   name = productivity_figure2,
  #   command = make_vegetation_figure(dat = productivity_prediction2,
  #                                    x_axis = Nitrogen_log,
  #                                    yaxislabel = bquote(Annual~productivity~g~m^-2~y^-1),
  #                                    colourpalette = col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = NA) +
  #     labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1)) +
  #     # add stats
  #     geom_text(data = productivity_prediction2 |>
  #                 distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
  #                 left_join(productivity_text2, by = "origSiteID"),
  #               aes(x = Inf, y = Inf, label = term),
  #               size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)
  # ),
  #
  #
  # ### COVER
  # # Functional group cover - grazing intensity
  # # text
  # tar_target(
  #   name = cover_text2,
  #   command = cover_stats2 |>
  #     mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
  #                                     term == "Intercept" ~ "non-sign",
  #                                     TRUE ~ "sign")) |>
  #     filter(significance == "sign") |>
  #     distinct(origSiteID, functional_group, term) |>
  #     mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))
  # ),
  #
  # # figure
  # tar_target(
  #   name = cover_figure2,
  #   command = make_vegetation_figure(dat = cover_prediction2,
  #                                    x_axis = Nitrogen_log,
  #                                    yaxislabel = "Change in cover",
  #                                    colourpalette = col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = "functional_group") +
  #     labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1)) +
  #     # add stats
  #     geom_text(data = cover_prediction2 |>
  #                 distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
  #                 left_join(cover_text2, by = c("origSiteID", "functional_group")) |>
  #                 mutate(term = if_else(is.na(term), "", term)),
  #               aes(x = Inf, y = Inf, label = term),
  #               size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)
  # ),

  ### DIVERSITY
  # grazing intensity
  # text
  tar_target(
    name = diversity_text2,
    command = diversity_stats2 |>
      mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                      term == "Intercept" ~ "non-sign",
                                      TRUE ~ "sign")) |>
      ### BY HAND CODE!!!
      filter(significance == "sign",
             term == "WxGxN") |>
      mutate(term = recode(term,
                           "WxGxN" = "W + WxN + WxGxN")) |>
      distinct(origSiteID, diversity_index, term) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),

  # figure
  tar_target(
    name = diversity_figure2,
    command = make_vegetation_figure(dat = diversity_prediction2,
                                     x_axis = Nitrogen_log,
                                     yaxislabel = "Change in diversity index",
                                     colourpalette = col_palette,
                                     linetypepalette = c("solid", "dashed", "dotted"),
                                     shapepalette = c(16, 0, 2),
                                     facet_2 = "diversity_index")  +
      labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1)) +
      facet_grid2(origSiteID ~ diversity_index, scales = "free_y", independent = "y") +
      # add stats
      geom_text(data = diversity_prediction2 |>
                  distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                  left_join(diversity_text2, by = c("origSiteID", "diversity_index")),
                aes(x = Inf, y = Inf, label = term),
                size = 4, colour = text_colour, hjust = 1.2, vjust = 1.4)
  )

)



