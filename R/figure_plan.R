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



  ### BIOMASS
  # biomass and cover figure in one
  tar_target(
    name = bio_cover_figure,
    command = {

      biomass_text <- biomass_anova_table |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "G", "WxN", "WxG", "GxN", "WxGxN"))) |>
        group_by(origSiteID, significance)

      biomass_figure <- make_vegetation_figure(dat1 = biomass_output |>
                                                      unnest(data) |>
                                                      mutate(biomass = "biomass"),
                                                    x_axis = Nitrogen_log,
                                                    yaxislabel = bquote(Standing~biomass~g~m^-2),
                                                    colourpalette = col_palette,
                                                    linetypepalette = c("solid", "dashed", "dotted"),
                                                    shapepalette = c(16, 0, 2),
                                                    facet_2 = "biomass",
                                                    dat2 = biomass_prediction) +
        labs(x = "", tag = "a)") +
        # add stats
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(1), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(2), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 3, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(3), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 4.6, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(4), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 6.2, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(5), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 7.8, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(6), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 9.4, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = biomass_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(biomass_text |>
                                filter(significance == "sign") |>
                                slice(7), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 11, label = term),
                  size = 3, colour = text_colour)
        # geom_text(data = productivity_prediction |>
        #             distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
        #             left_join(productivity_text |>
        #                         filter(significance == "marginal") |>
        #                         slice(1), by = "origSiteID"),
        #           aes(x = -Inf, y = Inf, hjust = 0, vjust = 7.8, label = term),
        #           size = 3, colour = "grey50")


      cover_text <- cover_anova_table |>
        filter(functional_group %in% c("graminoid", "forb")) |>
        filter(p.value <= 0.05) |>
        distinct(origSiteID, functional_group, term) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb"))) |>
        mutate(x_var = -Inf,
               y_var = if_else(functional_group == "graminoid", -Inf, Inf),
               hjust_var = 0,
               vjust_var = case_when(functional_group == "forb" & term == "W" ~ 1.4,
                                     functional_group == "forb" & term == "WxN" ~ 3,
                                     functional_group == "graminoid" & term == "W" ~ -4,
                                     functional_group == "graminoid" & term == "N" ~ -2.4,
                                     functional_group == "graminoid" & term == "G" ~ -0.8))

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
        theme(strip.background.y = element_blank(),
              strip.text.y = element_blank(),
              plot.margin = margin(r = 1, unit = "pt")) +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_text, by = c("origSiteID", "functional_group")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = text_colour)


      # sedges
      sedge_text <- cover_anova_table |>
        filter(functional_group == "sedge") |>
        filter(p.value <= 0.05) |>
        distinct(origSiteID, functional_group, term)

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
              plot.margin = margin(b = 1, l = 1, unit = "pt")) +
        geom_text(data = cover |>
                    filter(functional_group == "sedge") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sedge_text, by = c("origSiteID", "functional_group")),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.6, label = term),
                  size = 3, colour = text_colour)


      # legumes
      legume_text <- cover_anova_table |>
        filter(functional_group == "legume") |>
        filter(p.value <= 0.05) |>
        distinct(origSiteID, functional_group, term)

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
        theme(plot.margin = margin(l = 1, unit = "pt")) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(legume_text, by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 1.6, label = term),
                  size = 3, colour = text_colour)

      biomass_figure + cover_figure + sedge_figure/legumes_figure +
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
      diversity_text <- diversity_anova_table |>
        filter(p.value <= 0.07) |>
        mutate(sign = if_else(p.value <= 0.05, "sign", "marginal")) |>
        distinct(origSiteID, diversity_index, term, sign) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness"))) |>
        mutate(x_var = if_else(diversity_index == "richness", Inf, -Inf),
               y_var = -Inf,
               hjust_var = if_else(diversity_index == "richness", 1, 0))

      sign_text <- diversity_text |>
        filter(sign == "sign") |>
        mutate(vjust_var = case_when(origSiteID == "Alpine" & diversity_index == "richness" & term == "N" ~ -2.4,
                                     origSiteID == "Alpine" & term == "N" ~ -4,
                                     origSiteID == "Alpine" & term == "WxN" ~ -2.4,
                                     origSiteID == "Alpine" & term == "WxGxN" ~ -0.8,
                                     origSiteID == "Sub-alpine" & term == "W" ~ -2.4,
                                     origSiteID == "Sub-alpine" & term == "N" ~ -0.8))

      marg_text <- diversity_text |>
        filter(sign == "marginal") |>
        mutate(vjust_var = case_when(term == "W" ~ -4,
                                     term == "WxN" ~ -0.8))


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
                    left_join(sign_text, by = c("origSiteID", "diversity_index")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = diversity_prediction |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marg_text, by = c("origSiteID", "diversity_index")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = "grey50")

    }

  ),


  ### SUMMARY FIGURE
  tar_target(
    name = summary_figure,
    command = {

      # productivity vs diversity
      dat1 <- diversity |>
        select(-`2019`, -delta) |>
        rename(diversity = `2022`) |>
        filter(diversity_index == "diversity",
               grazing != "Natural") |>
        left_join(productivity |>
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

      plot2 <- ggplot(dat2, aes(x = Nitrogen_log, y = value, colour = warming, fill = warming, linetype = grazing)) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.1, linewidth = 0.5) +
        scale_colour_manual(name = "Warming", values = col_palette) +
        scale_fill_manual(name = "Warming", values = col_palette) +
        scale_linetype_manual(name = "Grazing", values = c("dashed", "dotted"),
                              guide = guide_legend(override.aes = list(color = "black") )) +
        # change labels to real values
        scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
        labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
             y = "Effect of grazing on diversity",
             tag = "b)") +
        lims(y = c(-1.2, 1.2)) +
        facet_wrap(~ origSiteID, nrow = 2) +
        theme_bw() +
        theme(legend.position = "top",
              legend.box="vertical")

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
        theme_bw() +
        theme(legend.position = "top",
              axis.text.x = element_text(size = 8))


      plot1 / (plot2 + plot3) + plot_layout(heights = c(1, 2))
      # plot_layout <- "
      # 1
      # 2
      # 2
      # 3
      # 3
      # "
      #
      # plot1 / plot2 / plot3 + plot_layout(design = plot_layout)

    }
  )

  # biomass - diversity figure
  # tar_target(
  #   name = bio_div_figure,
  #   command = biomass_diversity |>
  #     ggplot(aes(x = biomass, y = diversity, colour = warming, shape = grazing, size = Namount_kg_ha_y)) +
  #     geom_point() +
  #     #geom_smooth(method = "lm", formula = "y ~ x", colour = warming, shape = grazing) +
  #     # scales
  #     scale_colour_manual(name = "Warming", values = c("grey30", "#FD6467")) +
  #     scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  #     labs(x = bquote(Standing~biomass~g~m^-2),
  #          y = "Diversity") +
  #     facet_grid(~origSiteID) +
  #     theme_bw() +
  #     theme(legend.position = "top")

)
