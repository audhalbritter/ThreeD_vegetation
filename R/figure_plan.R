# figure plan

figure_plan <- list(

  ### Figure 3
  # standing biomass and drivers
  tar_target(
    name = bio_div_figure,
    command = {

      ### Figure 3a Biomass by origin
      biomass_text2 <- biomass_origin_anova_table |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))

      biomass_text3 <- biomass_origin_anova_table |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))


      bio <- make_vegetation_figure(dat1 = biomass_origin_output |>
                                      unnest(data),
                                   x_axis = Nitrogen_log,
                                   yaxislabel = bquote(Standing~biomass~g~m^-2),
                                   colourpalette = col_palette,
                                   linetypepalette = c("solid", "dashed", "dotted"),
                                   shapepalette = c(21, 22, 24),
                                   facet_2 = NA,
                                   # predictions
                                   dat2 = biomass_origin_prediction) +
        labs(tag = "a)") +
        # add stats
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(1),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(2),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(3),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 4.6, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(4),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 6.2, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text3 |>
                                group_by(origSiteID) |>
                                slice(4),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 6.2, label = term),
                  size = 3, colour = "grey50", nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(5),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 7.8, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = biomass_origin_prediction |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(biomass_text2 |>
                                group_by(origSiteID) |>
                                slice(6),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 9.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50)

      ### Figure 3b DIVERSITY BY ORIGIN
      div_text2 <- diversity_origin_anova_table |>
        filter(diversity_index == "diversity") |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))

        
        div <- make_vegetation_figure(dat1 = diversity_origin_output |>
                                      filter(diversity_index == "diversity") |>
                                      unnest(data),
                                      x_axis = Nitrogen_log,
                                      yaxislabel = "Shannon diversity",
                                      colourpalette = col_palette,
                                      linetypepalette = c("solid", "dashed", "dotted"),
                                      shapepalette = c(21, 22, 24),
                                      facet_2 = NA,
                                      # predictions
                                      dat2 = diversity_origin_prediction |>
                                      filter(diversity_index == "diversity")) +
        labs(tag = "b)") +
        # add stats
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "diversity") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(div_text2 |>
                                group_by(origSiteID) |>
                                slice(1),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.2, vjust = -1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "diversity") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(div_text2 |>
                                group_by(origSiteID) |>
                                slice(2),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.3, vjust = -3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "diversity") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(div_text2 |>
                                group_by(origSiteID) |>
                                slice(3),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.05, vjust = -4.6, label = term),
                  size = 3, colour = text_colour, nudge_x = 50)

      (bio + div) + plot_layout(guides = "collect") &
        theme(legend.position = "top",
              plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
              legend.background = element_rect(fill = "transparent"))

    }
  ),



  tar_target(
    name = standingB_div_final_figure,
    command = {

      biomass_div |>
        ggplot(aes(x = log(final_bio), y = final_diversity)) +
        geom_line(data = standingB_div_final_prediction |>
                          mutate(biomass_log = log(final_bio)), 
                        aes(y = .fitted,
                            x = biomass_log)) +
        geom_point(data = biomass_div, aes(colour = warming,
                                           shape = grazing,
                                           stroke = 0.8,
                                           size = Namount_kg_ha_y)) +
        scale_colour_manual(values = col_palette, name = "Warming") +
        scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +
        scale_size_continuous(name = "Nitrogen") +
        scale_linetype_manual(values = c("solid", "dashed"),
                              name = "Origin") +
        labs(x = bquote(Log(Standing~biomass)~g~m^-2),
             y = "Shannon diversity") +
        facet_wrap(vars(origSiteID)) +
        theme_bw() +
        theme(legend.position = "top",
              legend.box = "vertical",
              text = element_text(size = 12))
    }
  ),


  # Grazing effect on diversity dependent on biomass
  tar_target(
    name = grazing_div_figure,
    command = {

      # Grazing effect on diversity
      dat2 <- cover_total %>%
        filter(grazing != "Natural",
               year == 2022) |>
        group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log) %>%
        summarise(diversity = diversity(cover), .groups = "drop",
                  richness = n(),
                  evenness = diversity/log(richness)) |>

        # average for 0 kg N treatment
        group_by(origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log) |>
        summarise(value = mean(diversity), .groups = "drop") |>

        pivot_wider(names_from = grazing, values_from = value) %>%
        mutate(Intensive = Intensive - Control,
               Medium = Medium - Control) |>
        pivot_longer(cols = c("Medium", "Intensive"), names_to = "grazing", values_to = "value") |>
        mutate(grazing = factor(grazing, levels = c("Medium", "Intensive"))) |>
        # add biomass data
        tidylog::left_join(standing_biomass_back |>
                    ungroup() |>
                    filter(grazing == "Control") |>
                    select(-grazing),
                  by = c('origSiteID', 'warming', 'Namount_kg_ha_y', 'Nitrogen_log'))

      # test
      dat2 |>
        group_by(origSiteID) |>
        nest() |>
        mutate(model1 = map(.x = data, .f = ~ lm(value ~ standing_biomass * warming * Namount_kg_ha_y, data = .x)),
               model0 = map(.x = data, .f = ~ lm(value ~ standing_biomass, data = .x)),
               #glance1 = map(.x = model1, .f = glance),
               #glance0 = map(.x = model0, .f = glance),
               # result = map(.x = model, .f = tidy),
               anova1 = map(.x = model1, .f = car::Anova),
               anova_tidy1 = map(.x = anova1, .f = tidy),
               anova0 = map(.x = model0, .f = car::Anova),
               anova_tidy0 = map(.x = anova0, .f = tidy)
        ) |>
        unnest(anova_tidy1) |>
        select(origSiteID, term:p.value)

      dat2 |>
        ggplot(aes(x = log(standing_biomass), y = value)) +
        geom_point(mapping = aes(colour = warming, size = Nitrogen_log,
                                 shape = grazing)) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.1, linewidth = 0.5,
                    mapping = aes(colour = warming, fill = warming, linetype = grazing)) +
        #stat_cor(label.x = 0.2, label.y = 0.99) +
        scale_colour_manual(name = "Warming", values = col_palette) +
        scale_fill_manual(name = "Warming", values = col_palette) +
        scale_shape_manual(name = "Grazing", values = c(0, 2)) +
        scale_size(name = "log(Nitrogen)", range = c(1, 3)) +
        scale_linetype_manual(name = "Grazing", values = c("dashed", "dotted"),
                              guide = guide_legend(override.aes = list(color = "black") )) +
        labs(x = bquote(Biomass~g~m^-2),
             y = "Effect of grazing on diversity") +
        #guides(colour = "none", fill = "none", shape = "none", linetype = "none") +
        facet_wrap(~ origSiteID, nrow = 2) +
        theme_bw() +
        theme(legend.position = "bottom", legend.box="vertical")


    }
  )

)
