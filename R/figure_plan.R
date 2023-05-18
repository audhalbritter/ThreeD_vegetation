figure_plan <- list(

  # wes anderson colour palette
  tar_target(
    name = col_palette,
    #command = wes_palette("GrandBudapest1")[c(1, 2)]
    command = c("grey30", "#FD6467")
  ),

  #c("#FD6467", "#5B1A18", "#D67236")

  tar_target(
    name = text_colour,
    command = wes_palette("IsleofDogs2")[4]
  ),



  ### PRODUCTIVTY
  # productivity and cover figure in one
  tar_target(
    name = prod_cover_figure,
    command = {

      productivity_text <- productivity_stats |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance == "sign",
               !term %in% c("W", "G")) |>
        mutate(term = recode(term,
                             "WxN^2" = "W + WxN",
                             "N^2" = "G+N")) |>
        distinct(origSiteID, term)

      productivity_figure <- make_vegetation_figure(dat = productivity_prediction |>
                                                      mutate(productivity = "productivity"),
                                                    x_axis = Nitrogen_log,
                                                    yaxislabel = bquote(Annual~productivity~g~m^-2~y^-1),
                                                    colourpalette = col_palette,
                                                    linetypepalette = c("solid", "dashed", "dotted"),
                                                    shapepalette = c(16, 0, 2),
                                                    facet_2 = "productivity") +
        labs(x = "", tag = "a)") +
        # add stats
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text, by = "origSiteID"),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)


      cover_text <- cover_stats |>
        filter(functional_group %in% c("graminoid", "forb")) |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, functional_group, term) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))


      cover_figure <- make_vegetation_figure(dat = cover_prediction |>
                                               filter(functional_group %in% c("graminoid", "forb")),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group") +
        labs(tag = "b)") +
        # add stats
        geom_text(data = cover_prediction |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text, by = c("origSiteID", "functional_group")) |>
                    mutate(term = if_else(is.na(term), "", term)),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)


      sedge_figure <- make_vegetation_figure(dat = cover_prediction |>
                                               filter(functional_group == "sedge"),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group") +
        labs(x = "", y = "") +
        # add stats
        geom_text(data = cover_prediction |>
                    filter(functional_group == "sedge") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text, by = c("origSiteID", "functional_group")) |>
                    mutate(term = if_else(is.na(term), "", term)),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)

      legumes_figure <- make_vegetation_figure(dat = cover_prediction |>
                                               filter(functional_group == "legume"),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group") +
        labs(x = "", y = "") +
        # add stats
        geom_text(data = cover_prediction |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text, by = c("origSiteID", "functional_group")) |>
                    mutate(term = if_else(is.na(term), "", term)),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)

      productivity_figure + cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(1, 2, 1)) &
        theme(legend.position = "top")
        # guides(linetype = guide_legend(nrow = 2, byrow = TRUE),
        #        shape = guide_legend(nrow = 2, byrow = TRUE),
        #        colour = guide_legend(nrow = 2, byrow = TRUE))

    }
  ),

  # text
  # tar_target(
  #   name = productivity_text,
  #   command = productivity_stats |>
  #     mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
  #                                     term == "Intercept" ~ "non-sign",
  #                                     names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
  #                                     TRUE ~ "sign")) |>
  #     # BY HAND CODE!!!
  #     filter(significance == "sign",
  #            !term %in% c("W", "G")) |>
  #     mutate(term = recode(term,
  #                          "WxN^2" = "WxN",
  #                          "N^2" = "G+N")) |>
  #     distinct(origSiteID, term)
  # ),
  #
  # # figure
  # tar_target(
  #   name = productivity_figure,
  #   command = make_vegetation_figure(dat = productivity_prediction,
  #                                    x_axis = Namount_kg_ha_y,
  #                                    yaxislabel = bquote(Annual~productivity~g~m^-2~y^-1),
  #                                    colourpalette = col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = NA) +
  #     # add stats
  #     geom_text(data = productivity_prediction |>
  #                 distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
  #                 left_join(productivity_text, by = "origSiteID"),
  #               aes(x = Inf, y = Inf, label = term),
  #               size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)
  # ),
  #
  # ### COVER
  # # Functional group cover - grazing intensity
  # # text
  # tar_target(
  #   name = cover_text,
  #   command = cover_stats |>
  #     mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
  #                                     term == "Intercept" ~ "non-sign",
  #                                     names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
  #                                     TRUE ~ "sign")) |>
  #     filter(significance == "sign") |>
  #     distinct(origSiteID, functional_group, term) |>
  #     mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))
  # ),
  #
  # # figure
  # tar_target(
  #   name = cover_figure,
  #   command = make_vegetation_figure(dat = cover_prediction,
  #                                    x_axis = Namount_kg_ha_y,
  #                                    yaxislabel = "Change in cover",
  #                                    colourpalette = col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = "functional_group") +
  #     # add stats
  #     geom_text(data = cover_prediction |>
  #                 distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
  #                 left_join(cover_text, by = c("origSiteID", "functional_group")) |>
  #                 mutate(term = if_else(is.na(term), "", term)),
  #               aes(x = Inf, y = Inf, label = term),
  #               size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)
  # ),

  # Bryo, Liche and litter - grazing intensity
  # text
  # tar_target(
  #   name = cover_bryo_text,
  #   command = cover_bryo_stats |>
  #     mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
  #                                     term == "Intercept" ~ "non-sign",
  #                                     names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
  #                                     TRUE ~ "sign")) |>
  #     filter(significance == "sign") |>
  #     mutate(term = recode(term,
  #                          "WxN^2" = "WxN",
  #                          "N^2" = "N",
  #                          "GxN^2" = "W+GxN")) |>
  #     distinct(origSiteID, functional_group, term) |>
  #     # removed by hand!!!
  #     filter(!(origSiteID == "Alpine" & term == "W")) |>
  #     filter(!(origSiteID == "Sub-alpine" & term == "W")) |>
  #     filter(!(origSiteID == "Sub-alpine" & functional_group == "Litter" & term == "G")) |>
  #     filter(!(origSiteID == "Sub-alpine" & term == "N")) |>
  #     mutate(functional_group = factor(functional_group, levels = c("Bryophytes", "Lichen", "Litter")))
  # ),
  #
  # # figure
  # tar_target(
  #   name = cover_bryo_figure,
  #   command = make_vegetation_figure(dat = cover_bryo_prediction,
  #                                    x_axis = Namount_kg_ha_y,
  #                                    yaxislabel = "Change in cover",
  #                                    colourpalette = col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = "functional_group") +
  #     # independent axis
  #     facet_grid2(origSiteID ~ functional_group, scales = "free_y", independent = "y") +
  #   # add stats
  #   geom_text(data = cover_bryo_prediction |>
  #               distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
  #               left_join(cover_bryo_text, by = c("origSiteID", "functional_group")) |>
  #               mutate(term = if_else(is.na(term), "", term)),
  #             aes(x = Inf, y = Inf, label = term),
  #             size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)
  # ),



  ### DIVERSITY
  # grazing intensity
  # text
  tar_target(
    name = diversity_text,
    command = diversity_stats |>
      mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                      term == "Intercept" ~ "non-sign",
                                      names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                      TRUE ~ "sign")) |>
      filter(significance == "sign") |>
      mutate(term = recode(term,
                           "WxGxN^2" = "WxGxN")) |>
      distinct(origSiteID, diversity_index, term) |>
      # added trend by hand!!!
      bind_rows(tibble(origSiteID = "Sub-alpine",
                       diversity_index = "evenness",
                       term = "~N")) |>
      bind_rows(tibble(origSiteID = "Sub-alpine",
                       diversity_index = "diversity",
                       term = "~N")) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),

  # figure
  tar_target(
    name = diversity_figure,
    command = make_vegetation_figure(dat = diversity_prediction,
                                     x_axis = Nitrogen_log,
                                     yaxislabel = "Change in diversity index",
                                     colourpalette = col_palette,
                                     linetypepalette = c("solid", "dashed", "dotted"),
                                     shapepalette = c(16, 0, 2),
                                     facet_2 = "diversity_index")  +
      labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1)) +
      facet_grid2(origSiteID ~ diversity_index, scales = "free_y", independent = "y") +
      # add stats
      geom_text(data = diversity_prediction |>
                  distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                  left_join(diversity_text, by = c("origSiteID", "diversity_index")),
                aes(x = Inf, y = Inf, label = term),
                size = 4, colour = text_colour, hjust = 1.2, vjust = 1.4)
  )

)



