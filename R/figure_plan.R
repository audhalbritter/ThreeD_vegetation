# figure plan

figure_plan <- list(

  # Biomass vs diversity figure
  tar_target(
    name = standingB_div_figure,
    command = {

      dat <- cover_total %>%
        filter(year == 2022) |>
        ungroup() |>
        group_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log) %>%
        summarise(richness = n(),
                  diversity = diversity(cover),
                  evenness = diversity/log(richness)) %>%
        # add standing biomass
        tidylog::left_join(standing_biomass_back,
                  by = c('origSiteID', 'warming', "grazing", "Nlevel", 'Namount_kg_ha_y', 'Nitrogen_log'))


      # biomass_div |>
      #   ungroup() |>
      #   select(delta_diversity, warming, grazing, Namount_kg_ha_y, standing_biomass) |>
      dat |>
        ggplot(aes(x = log(standing_biomass), y = diversity,
                   colour = warming, shape = grazing, size = Namount_kg_ha_y)) +
        geom_point() +
        scale_colour_manual(values = col_palette, name = "Warming") +
        scale_shape_manual(values = c(16, 0, 2, 5), name = "Grazing") +
        scale_size_continuous(name = "Nitrogen") +
        labs(x = bquote(Log(Standing~biomass)~g~m^-2),
             y = "Shannon diversity") +
        theme_bw() +
        theme(legend.position = "bottom", legend.box="vertical")
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
        unnest(anova_tidy0) |>
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
