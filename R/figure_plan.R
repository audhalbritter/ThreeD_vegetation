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
        mutate(significance = case_when(p.value >= 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance == "sign",
               !term %in% c("W", "G")) |>
        mutate(term = recode(term,
                             "WxN²" = "W",
                             "N²" = "G")) |>
        distinct(origSiteID, term)

      productivity_text2 <- productivity_text |>
        mutate(term = recode(term,
                             "W" = "WxN²",
                             "G" = "N²"))

      productivity_marginal <- productivity_text |>
        mutate(term = recode(term,
                             "W" = "WxG",
                             "G" = "WxN²"))

      productivity_figure <- make_vegetation_figure(dat1 = productivity_output |>
                                                      unnest(data) |>
                                                      mutate(productivity = "productivity"),
                                                    x_axis = Nitrogen_log,
                                                    yaxislabel = bquote(Annual~productivity~g~m^-2~y^-1),
                                                    colourpalette = col_palette,
                                                    linetypepalette = c("solid", "dashed", "dotted"),
                                                    shapepalette = c(16, 0, 2),
                                                    facet_2 = "productivity",
                                                    dat2 = productivity_prediction) +
        labs(x = "", tag = "a)") +
        # add stats
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text, by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text2, by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 3, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_marginal, by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 4.6, label = term),
                    size = 3, colour = "grey50")


      cover_text <- cover_stats |>
        filter(functional_group %in% c("graminoid", "forb")) |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, functional_group, term) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))

      cover <- cover_output |>
        unnest(data) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))

      cover_figure <- make_vegetation_figure(dat = cover |>
                                               filter(functional_group %in% c("graminoid", "forb")),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in percentage cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group",
                                             dat2 = cover_prediction |>
                                               filter(functional_group %in% c("graminoid", "forb"))) +
        labs(tag = "b)") +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text, by = c("origSiteID", "functional_group")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -1.2, label = term),
                  size = 3, colour = text_colour) +
        theme(strip.background.y = element_blank(),
              strip.text.y = element_blank(),
              plot.margin = margin(r = 1, unit = "pt"))


      sedge_figure <- make_vegetation_figure(dat1 = cover |>
                                               filter(functional_group == "sedge"),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group",
                                             dat2 = cover_prediction |>
                                               filter(functional_group == "sedge")) +
        labs(x = "", y = "") +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(b = 1, l = 1, unit = "pt"))


      # forb <- grid::rasterGrob(png::readPNG("forb.png"), interpolate = TRUE)
      # sedge_figure +
      #   annotation_custom(forb, xmin = -2, xmax = 10, ymin = -25, ymax = -30) +
      #   coord_cartesian(clip = "off")

      legumes_figure <- make_vegetation_figure(dat1 = cover |>
                                               filter(functional_group == "legume"),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group",
                                             dat2 = cover_prediction |>
                                               filter(functional_group == "legume")) +
        labs(x = "", y = "") +
        theme(plot.margin = margin(l = 1, unit = "pt"))

      productivity_figure + cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(1, 2, 1)) &
        theme(legend.position = "top")

    }
  ),



  ### DIVERSITY
  # grazing intensity

  tar_target(
    name = diversity_figure,
    command = {

      # text
      diversity_text <- diversity_stats |>
        mutate(significance = case_when(p.value >= 0.0502 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, diversity_index, term) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

      marginal_text <- diversity_text |>
        mutate(term = if_else(origSiteID == "Alpine", "WxG", term),
               term = if_else(origSiteID == "Sub-alpine", "", term)) |>
        bind_rows(tibble(origSiteID = "Sub-alpine",
                         diversity_index = "evenness",
                         term = "N")) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

      marginal_text2 <- marginal_text |>
        mutate(term = if_else(origSiteID == "Alpine" & diversity_index == "diversity", "WxN²", "")) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))





      # figure
      make_vegetation_figure(dat1 = diversity_output |>
                                       unnest(data),
                             x_axis = Nitrogen_log,
                             yaxislabel = "Change in diversity index",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed", "dotted"),
                             shapepalette = c(16, 0, 2),
                             facet_2 = "diversity_index",
                             dat2 = diversity_prediction)  +
        labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1)) +
        facet_grid2(origSiteID ~ diversity_index, scales = "free_y", independent = "y") +
        # add stats
        geom_text(data = diversity_prediction |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(diversity_text, by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -3, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = diversity_prediction |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marginal_text, by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -1.6, label = term),
                  size = 3, colour = "grey50") +
        geom_text(data = diversity_prediction |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marginal_text2, by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -0.2, label = term),
                  size = 3, colour = "grey50")

    }

  ),


  tar_target(
    name = summary_figure,
    command = {

      # productivity vs diversity
      dat1 <- diversity |>
        select(-`2019`, -delta) |>
        rename(diversity = `2022`) |>
        filter(diversity_index == "diversity",
               grazing != "Natural") |>
        tidylog::left_join(productivity |>
                             filter(year == "2022",
                                    fun_group != "litter") |>
                             ungroup() |>
                             group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num) |>
                             summarise(sum_productivity = sum(productivity)),
                           by = c('origSiteID', "grazing", "grazing_num", 'warming', 'Namount_kg_ha_y', 'Nitrogen_log'))

      dat1 |>
        group_by() |>
        nest() |>
        mutate(fit = map(.x = data, .f = ~lm(diversity ~ sum_productivity, data = .)),
               result = map(fit, tidy)) |>
        unnest(result)
      check_model(lm(diversity ~ sum_productivity, data = dat1))

      plot1 <- ggplot(dat1, aes(x = sum_productivity, y = diversity)) +
        geom_point() +
        geom_smooth(method = "lm", formula = "y ~ x", colour = "grey30") +
        labs(x = bquote(Productivity~g~m^-2~y^-1),
             y = "Diversity",
             tag = "a)") +
        theme_bw() +
        theme(legend.position = "top")

      # Grazing effect on diversity
      dat2 <- cover %>%
        filter(grazing != "Natural",
               year == 2022) |>
        group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log) %>%
        summarise(diversity = diversity(cover)) |>

        # average for 0 kg N treatment
        ungroup() |>
        group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log) |>
        summarise(value = mean(diversity)) |>

        pivot_wider(names_from = grazing, values_from = value) %>%
        mutate(Intensive = Intensive - Control,
               Medium = Medium - Control) |>
        pivot_longer(cols = c("Medium", "Intensive"), names_to = "grazing", values_to = "value") |>
        mutate(grazing = factor(grazing, levels = c("Medium", "Intensive")))

      plot2 <- ggplot(dat2, aes(x = Nitrogen_log, y = value, colour = warming, fill = warming, shape = grazing, linetype = grazing)) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.1, linewidth = 0.5) +
        scale_colour_manual(name = "Warming", values = col_palette) +
        scale_fill_manual(name = "Warming", values = col_palette) +
        scale_linetype_manual(name = "Grazing", values = c("dashed", "dotted")) +
        scale_shape_manual(name = "Grazing", values = c(0, 2)) +
        # change labels to real values
        scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
        labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
             y = "Effect of grazing on diversity",
             tag = "b)") +
        lims(y = c(-1.2, 1.2)) +
        facet_wrap(~ origSiteID, nrow = 2) +
        theme_bw()

      # interactions
      plot3 <- single_vs_interaction |>
        mutate(group = factor(group, levels = c("productivity", "graminoid", "forb", "sedge", "legume", "richness", "diversity", "evenness"))) |>
        filter(variable != "cover") |>
        ggplot(aes(x = group, y = adj.r.squared)) +
        geom_col(aes(group = effects, fill = effects), position = "dodge") +
        scale_fill_manual(values = c("plum3", "darkgoldenrod")) +
        labs(x = "", y = "Adjusted R squared",
             tag = "c)") +
        facet_wrap(~origSiteID, nrow = 2) +
        theme_bw()


      plot_layout <- "
      1
      2
      2
      3
      3
      "

      plot1 / plot2 / plot3 + plot_layout(design = plot_layout)

    }
  )

)



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
