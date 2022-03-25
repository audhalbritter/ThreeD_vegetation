figure_plan <- list(
  # site map
  tar_target(
    name = site_map,
    command = {
      world_df <- map_data("world")

      ggplot(world_df, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "#8AAAA9") +
        geom_point(aes(x = 7.16990, y = 60.88019), colour = "#5B424B") +
        geom_point(aes(x = 102.036, y = 29.8619), colour = "#5B424B") +
        labs(x = NULL, y = NULL) +
        coord_sf(xlim = c(0, 110), ylim = c(10, 72), expand = FALSE) +
        theme_void()
    }),

  tar_target(
    name = soil_plot,
    command = {
      soil %>%
        left_join(meta_soil %>%
                    mutate(destBlockID = as.character(destBlockID)) %>%
                    group_by(destSiteID, destBlockID, year) %>%
                    summarise(soil_depth_cm = mean(soil_depth_cm)),
                  by = c("destSiteID", "destBlockID", "year")) %>%
        pivot_longer(cols = c(pH, bulk_density_g_cm, soil_organic_matter, soil_depth_cm, C_percent, N_percent),
                     names_to = "variable", values_to = "value") %>%
        mutate(variable = factor(variable, levels = c("soil_depth_cm", "bulk_density_g_cm", "pH", "soil_organic_matter", "C_percent", "N_percent")),
               Site = recode(destSiteID, Lia = "High alpine", Joa = "Alpine", Vik = "Lowland"),
               Site = factor(Site, levels = c("Lowland", "Alpine", "High alpine")),
               layer = factor(layer, levels = c("Top", "Bottom")),
               value = if_else(variable == "soil_depth_cm" & layer == "Bottom", NA_real_, value)) %>%
        filter(!is.na(value)) %>%
        ggplot(aes(x = Site, y = value, fill = layer)) +
        geom_boxplot() +
        scale_fill_manual(name = "", values = c("rosybrown1", "rosybrown")) +
        labs(x = "", y = "") +
        facet_wrap(~ variable, scales = "free_y") +
        theme(legend.position = "top")

    }),


  # richness
  tar_target(
    name = richness_plot,
    command = {

      sign <- diversity_analysis |>
        mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
        filter(diversity_index == "richness",
               p.value <= 0.05) |>
        mutate(sign = paste(term, "sign", sep = "_")) |>
        select(diversity_index, origSiteID:grazing, term, sign)

      # 3 colour palette (grey, green, orange)
      three_col_palette <- c("#999999", "#009E73", "#E69F00")

      richness <- diversity_data |>
        filter(diversity_index == "richness") |>
        #distinct(origSiteID, warming, grazing) |>
        left_join(sign, by = c("origSiteID", "grazing")) |>
        mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
                                term == "N" & warming == "warming" ~ NA_character_,
                                TRUE ~ sign),
               sign = if_else(is.na(sign),  "no effect", sign),
               sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming"),
               sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming"))) |>
        ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
        geom_point(size = 2) +
        labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = "Change in richness") +
        scale_colour_manual(name = "Effect", values = three_col_palette) +
        scale_fill_manual(name = "Effect", values = three_col_palette) +
        scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
        scale_shape_manual(name = "Warming", values = c(1, 16)) +
        facet_grid(origSiteID ~ grazing) +
        theme_minimal() +
        theme(text = element_text(size = 16))


      ggsave(richness, filename = "output/richness.jpg", dpi = 300, width = 8, height = 6, bg = "white")
  }),

  # diversity
  tar_target(
    name = diversity_plot,
    command = {

      sign <- diversity_analysis |>
        mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
        filter(diversity_index == "diversity",
               p.value <= 0.05,
               term != "Intercept") |>
        mutate(sign = paste(term, "sign", sep = "_")) |>
        select(diversity_index, origSiteID:grazing, term, sign)

      # 4 colour palette (grey, green, orange, pink)
      four_col_palette <- c("#999999", "#009E73", "#E69F00", "#CC79A7")


      diversity <- diversity_data |>
        filter(diversity_index == "diversity") |>
        #distinct(origSiteID, warming, grazing) |>
        left_join(sign, by = c("origSiteID", "grazing")) |>
        mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
                                term == "N" & warming == "warming" ~ NA_character_,
                                TRUE ~ sign),
               sign = if_else(is.na(sign),  "no effect", sign),
               sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming", NxW_sign = "Interaction"),
               sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming", "Interaction"))) |>
        ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
        geom_point(size = 2) +
        labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = "Change in diversity") +
        scale_colour_manual(name = "Effect", values = four_col_palette) +
        scale_fill_manual(name = "Effect", values = four_col_palette) +
        scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
        scale_shape_manual(name = "warming", values = c(16, 1)) +
        facet_grid(origSiteID ~ grazing) +
        theme_minimal() +
        theme(text = element_text(size = 16))

      ggsave(diversity, filename = "output/diversity.jpg", dpi = 300, width = 8, height = 6, bg = "white")
    }),

  # evenness
  tar_target(
    name = evenness_plot,
    command = {

      sign <- diversity_analysis |>
        mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
        filter(diversity_index == "evenness",
               p.value <= 0.05,
               term != "Intercept") |>
        mutate(sign = paste(term, "sign", sep = "_")) |>
        select(diversity_index, origSiteID:grazing, term, sign)

      # 4 colour palette (grey, green)
      two_col_palette <- c("#999999", "#009E73")


      evenness <- diversity_data |>
        filter(diversity_index == "evenness") |>
        #distinct(origSiteID, warming, grazing) |>
        left_join(sign, by = c("origSiteID", "grazing")) |>
        mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
                                term == "N" & warming == "warming" ~ NA_character_,
                                TRUE ~ sign),
               sign = if_else(is.na(sign),  "no effect", sign),
               sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming", NxW_sign = "Interaction"),
               sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming", "Interaction"))) |>
        ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
        geom_point(size = 2) +
        labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = "Change in evenness") +
        scale_colour_manual(name = "Effect", values = two_col_palette) +
        scale_fill_manual(name = "Effect", values = two_col_palette) +
        scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
        scale_shape_manual(name = "warming", values = c(16, 1)) +
        facet_grid(origSiteID ~ grazing) +
        theme_minimal() +
        theme(text = element_text(size = 16))

      ggsave(evenness, filename = "output/evenness.jpg", dpi = 300, width = 8, height = 6, bg = "white")
    }),


  # cover
  tar_target(
    name = cover_plot,
    command = {

      sign <- cover_analysis |>
        mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
        filter(p.value <= 0.05,
               term != "Intercept") |>
        mutate(sign = paste(term, "sign", sep = "_")) |>
        select(origSiteID:functional_group, term, sign) |>
        filter(!c(grazing == "Control" & sign == "W_sign"))

      # 3 colour palette (grey, orange, pink)
      three_col_palette <- c("#999999", "#E69F00", "#CC79A7")

      cover_HA <- cover_data |>
        filter(functional_group %in% c("graminoid", "forb"),
               origSiteID == "High alpine") %>%
        left_join(sign, by = c("origSiteID", "grazing", "functional_group")) |>
        mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
                                term == "N" & warming == "warming" ~ NA_character_,
                                TRUE ~ sign),
               sign = if_else(is.na(sign),  "no effect", sign),
               sign = recode(sign, W_sign = "Warming", NxW_sign = "Interaction"),
               sign = factor(sign, levels = c("no effect", "Warming", "Interaction"))) |>
        ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
        geom_point(size = 2) +
        labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = "Change in cover",
             title = "High alpine") +
        scale_colour_manual(name = "Effect", values = three_col_palette) +
        scale_fill_manual(name = "Effect", values = three_col_palette) +
        scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
        scale_shape_manual(name = "Warming", values = c(1, 16)) +
        facet_grid(functional_group ~ grazing) +
        theme_minimal() +
        theme(text = element_text(size = 16))


      # 4 colour palette (grey, green, pink)
      three_col_palette <- c("#999999", "#009E73", "#CC79A7")

      cover_A <- cover_data |>
        filter(functional_group %in% c("graminoid", "forb"),
               origSiteID == "Alpine") %>%
        left_join(sign, by = c("origSiteID", "grazing", "functional_group")) |>
        mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
                                term == "N" & warming == "warming" ~ NA_character_,
                                TRUE ~ sign),
               sign = if_else(is.na(sign),  "no effect", sign),
               sign = recode(sign, N_sign = "Nitrogen", NxW_sign = "Interaction"),
               sign = factor(sign, levels = c("no effect", "Nitrogen", "Interaction"))) |>
        ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
        geom_point(size = 2) +
        labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = "Change in cover",
             title = "Alpine") +
        scale_colour_manual(name = "Effect", values = three_col_palette) +
        scale_fill_manual(name = "Effect", values = three_col_palette) +
        scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
        scale_shape_manual(name = "Warming", values = c(1, 16)) +
        facet_grid(functional_group ~ grazing) +
        theme_minimal() +
        theme(text = element_text(size = 16))


      # panel1 <- cover_data %>%
      #   filter(functional_group == "graminoid",
      #          grazing =="Control",
      #          origSiteID == "High alpine") %>%
      #   ggplot(aes(x = Nitrogen_log, y = delta, colour = functional_group, linetype = warming, shape = warming)) +
      #   geom_point() +
      #   geom_smooth(method = "lm", se = FALSE) +
      #   labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
      #        y = "Difference in cover (2021 - 2019)") +
      #   scale_colour_manual(name = "", values = c("limegreen")) +
      #   scale_linetype_manual(name = "warming", values = c("solid", "dashed")) +
      #   scale_shape_manual(name = "warming", values = c(16, 1)) +
      #   lims(y = c(-60, 80)) +
      #   theme_minimal()
      #
      # ggsave(panel1, filename = "output/panel1.jpg", dpi = 150, width = 3, height = 4, bg = "white")
      ggsave(cover_HA, filename = "output/cover_HA.jpg", dpi = 150, width = 8, height = 6, bg = "white")
      ggsave(cover_A, filename = "output/cover_A.jpg", dpi = 150, width = 8, height = 6, bg = "white")
    })#,

  # # biomass
  # tar_target(
  #   name = biomass_plot,
  #   command = {
  #     biomass_plot <- biomass_data %>%
  #       filter(fun_group %in% c("graminoids", "forbs", "cyperaceae", "legumes")) %>%
  #       mutate(site_warm = paste0(origSiteID, warming)) %>%
  #       ggplot(aes(x = log(Namount_kg_ha_y), y = productivity, colour = fun_group, linetype = warming, shape = warming)) +
  #       geom_point() +
  #       geom_smooth(method = "lm", se = FALSE) +
  #       labs(x = bquote(Nitrogen~(kg~ha^-1~y^-1)),
  #            y = "Productivity in g per year") +
  #       scale_colour_manual(name = "", values = c("orange", "plum4", "limegreen", "darkgreen")) +
  #       scale_linetype_manual(name = "warming", values = c("solid", "dashed")) +
  #       scale_shape_manual(name = "warming", values = c(16, 1)) +
  #       facet_grid(origSiteID ~ grazing, scales = "free_y") +
  #       theme_minimal()
  #
  #     ggsave(biomass_plot, filename = "output/biomass.jpg", dpi = 150, width = 8, height = 6, bg = "white")
  #   }),
  #
  #
  # # fun group biomass
  # tar_target(
  #   name = biomass_plot2,
  #   command = {
  #     biomass_data %>%
  #       mutate(site_warm = paste0(origSiteID, warming)) %>%
  #       ggplot(aes(x = Namount_kg_ha_y, y = biomass, colour = grazing, linetype = warming, shape = warming)) +
  #       geom_point() +
  #       geom_smooth(method = "lm", se = FALSE) +
  #       labs(x = bquote(Nitrogen~(kg~ha^-1~y^-1)),
  #            y = "Biomass in g") +
  #       #scale_colour_manual(name = "", values = c("plum4", "limegreen")) +
  #       scale_linetype_manual(name = "warming", values = c("solid", "dashed")) +
  #       scale_shape_manual(name = "warming", values = c(16, 1)) +
  #       facet_grid(fun_group ~ origSiteID, scales = "free_y")
  #   })

  )



