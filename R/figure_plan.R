figure_plan <- list(
  # site map
  # tar_target(
  #   name = site_map,
  #   command = {
  #     world_df <- map_data("world")
  #
  #     ggplot(world_df, aes(x = long, y = lat, group = group)) +
  #       geom_polygon(fill = "#8AAAA9") +
  #       geom_point(aes(x = 7.16990, y = 60.88019), colour = "#5B424B") +
  #       geom_point(aes(x = 102.036, y = 29.8619), colour = "#5B424B") +
  #       labs(x = NULL, y = NULL) +
  #       coord_sf(xlim = c(0, 110), ylim = c(10, 72), expand = FALSE) +
  #       theme_void()
  #   }),


  ### CLIMATE
  # annual climate figure
  tar_target(
    name = annual_climate_figure,
    command = make_annual_climate_figure(annual_climate)
  ),

  tar_target(
    name = daily_climate_figure,
    command = make_daily_climate_figure(daily_temp)
  ),


  ### BIOMASS
  tar_target(
    name = biomass_fun_group,
    command = make_functional_group_biomass_figure(biomass)
  ),

  tar_target(
    name = productivity_figure,
    command = make_productivity_figure(biomass)
  ),

  ### COVER
  # cover - alpine
  tar_target(
    name = cover_alpine_figure,
    command = {
      # grey, pink
      NxW_col_palette <- c("#999999", "#CC79A7")

      subset <- functional_group_cover |>
        filter(functional_group %in% c("graminoid", "forb"),
             origSiteID == "Alpine",
             grazing != "Natural") %>%
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb"))) |>
        #left_join(sign, by = c("origSiteID", "functional_group")) |>
        mutate(sign = if_else(functional_group == "graminoid" &
                                grazing == "Control",
                              "NxW", "no effect"))

      make_cover_fig(subset,
                     facet_1 = "functional_group",
                     fig_title = "Alpine",
                     colour_palette = NxW_col_palette)
    }

  ),

  # cover - sub-alpine
  tar_target(
    name = cover_subalpine_figure,
    command = {
      # grey, blue, orange
      three_col_palette <- c("#999999", "#0072B2", "#E69F00")

      subset <- functional_group_cover |>
        filter(functional_group %in% c("graminoid", "forb"),
               origSiteID == "Sub-alpine",
               grazing != "Natural") %>%
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb"))) |>
        mutate(sign = case_when(functional_group == "graminoid" & warming == "Ambient" ~ "GxN",
                                functional_group == "graminoid" & warming == "Warming" ~ "W",
                                TRUE ~ "no effect"),
               sign = factor(sign, levels = c("no effect", "GxN", "W")))

        make_cover_fig(subset,
                       facet_1 = "functional_group",
                       fig_title = "Sub-alpine",
                       colour_palette = three_col_palette)
    }

  ),

  # cover CN - alpine
  tar_target(
    name = cover_cn_alpine_figure,
    command = {
      # grey, light blue, orange
      G_W_col_palette <- c("#999999", "#56B4E9", "#E69F00")

      subset <- functional_group_cover |>
        filter(functional_group %in% c("graminoid", "forb"),
               origSiteID == "Alpine",
               grazing %in% c("Control", "Natural")) %>%
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb"))) |>
        mutate(sign = case_when(functional_group == "graminoid" & warming == "Warming" ~ "W",
                                functional_group == "forb" & grazing == "Natural" & warming == "Ambient"
                                ~ "G",                          TRUE ~ "no effect"),
               sign = factor(sign, levels = c("no effect", "G", "W")))



      make_cover_fig(subset,
                     facet_1 = "functional_group",
                     fig_title = "Alpine",
                     colour_palette = G_W_col_palette)
    }

  ),

  # cover CN - sub-alpine
  tar_target(
    name = cover_cn_subalpine_figure,
    command = {
      # grey, green, orange
      G_W_col_palette <- c("#999999", "#009E73", "#E69F00")

      subset <- functional_group_cover |>
        filter(functional_group %in% c("graminoid", "forb"),
               origSiteID == "Sub-alpine",
               grazing %in% c("Control", "Natural")) %>%
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb"))) |>
        mutate(sign = case_when(functional_group == "graminoid" & warming == "Warming" ~ "W",
                                functional_group == "graminoid" & warming == "Ambient" ~ "N",
                                TRUE ~ "no effect"),
               sign = factor(sign, levels = c("no effect", "N", "W")))



      make_cover_fig(subset,
                     facet_1 = "functional_group",
                     fig_title = "Sub-alpine",
                     colour_palette = G_W_col_palette)
    }

  ),


  # Diversity figures

  # species richness
  tar_target(
    name = richness_figure,
    command = {
      # grey, green, orange
      G_W_col_palette <- c("#999999", "#009E73", "#E69F00")

      model <- diversity_result |>
        filter(diversity_index == "richness") |>
        distinct(origSiteID, best_model) |>
        mutate(best_model = str_remove(best_model, "delta ~ "))

      subset <- diversity |>
        filter(diversity_index == "richness",
               grazing != "Natural") %>%
        mutate(sign = case_when(origSiteID == "Alpine" & warming == "Ambient" ~ "N",
                                origSiteID == "Sub-alpine" & warming == "Warming" ~ "W",
                                TRUE ~ "no effect"),
               sign = factor(sign, levels = c("no effect", "N", "W"))) |>
        left_join(model, by = c("origSiteID", "diversity_index"))

      make_cover_fig(subset,
                     facet_1 = "origSiteID",
                     fig_title = "Richness",
                     colour_palette = G_W_col_palette)
    }

  )





    # ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
    # geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
    # geom_point(size = 2) +
    # labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
    #      y = "Change in cover",
    #      title = "Alpine") +
    # scale_colour_manual(name = "Effect", values = three_col_palette) +
    # scale_fill_manual(name = "Effect", values = three_col_palette) +
    # scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
    # scale_shape_manual(name = "Warming", values = c(1, 16)) +
    # facet_grid(functional_group ~ grazing) +
    # theme_minimal() +
    # theme(text = element_text(size = 16))


  # tar_target(
  #   name = soil_plot,
  #   command = {
  #     soil %>%
  #       left_join(meta_soil %>%
  #                   mutate(destBlockID = as.character(destBlockID)) %>%
  #                   group_by(destSiteID, destBlockID, year) %>%
  #                   summarise(soil_depth_cm = mean(soil_depth_cm)),
  #                 by = c("destSiteID", "destBlockID", "year")) %>%
  #       pivot_longer(cols = c(pH, bulk_density_g_cm, soil_organic_matter, soil_depth_cm, C_percent, N_percent),
  #                    names_to = "variable", values_to = "value") %>%
  #       mutate(variable = factor(variable, levels = c("soil_depth_cm", "bulk_density_g_cm", "pH", "soil_organic_matter", "C_percent", "N_percent")),
  #              Site = recode(destSiteID, Lia = "High alpine", Joa = "Alpine", Vik = "Lowland"),
  #              Site = factor(Site, levels = c("Lowland", "Alpine", "High alpine")),
  #              layer = factor(layer, levels = c("Top", "Bottom")),
  #              value = if_else(variable == "soil_depth_cm" & layer == "Bottom", NA_real_, value)) %>%
  #       filter(!is.na(value)) %>%
  #       ggplot(aes(x = Site, y = value, fill = layer)) +
  #       geom_boxplot() +
  #       scale_fill_manual(name = "", values = c("rosybrown1", "rosybrown")) +
  #       labs(x = "", y = "") +
  #       facet_wrap(~ variable, scales = "free_y") +
  #       theme(legend.position = "top")
  #
  #   }),
  #
  #
  # # richness
  # tar_target(
  #   name = richness_plot,
  #   command = {
  #
  #     sign <- diversity_analysis |>
  #       mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
  #       filter(diversity_index == "richness",
  #              p.value <= 0.05) |>
  #       mutate(sign = paste(term, "sign", sep = "_")) |>
  #       select(diversity_index, origSiteID:grazing, term, sign)
  #
  #     # 3 colour palette (grey, green, orange)
  #     three_col_palette <- c("#999999", "#009E73", "#E69F00")
  #
  #     richness <- diversity_data |>
  #       filter(diversity_index == "richness") |>
  #       #distinct(origSiteID, warming, grazing) |>
  #       left_join(sign, by = c("origSiteID", "grazing")) |>
  #       mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
  #                               term == "N" & warming == "warming" ~ NA_character_,
  #                               TRUE ~ sign),
  #              sign = if_else(is.na(sign),  "no effect", sign),
  #              sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming"),
  #              sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming"))) |>
  #       ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
  #       geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
  #       geom_point(size = 2) +
  #       labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
  #            y = "Change in richness") +
  #       scale_colour_manual(name = "Effect", values = three_col_palette) +
  #       scale_fill_manual(name = "Effect", values = three_col_palette) +
  #       scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
  #       scale_shape_manual(name = "Warming", values = c(1, 16)) +
  #       facet_grid(origSiteID ~ grazing) +
  #       theme_minimal() +
  #       theme(text = element_text(size = 16))
  #
  #
  #     ggsave(richness, filename = "output/richness.jpg", dpi = 300, width = 8, height = 6, bg = "white")
  # }),
  #
  # # diversity
  # tar_target(
  #   name = diversity_plot,
  #   command = {
  #
  #     sign <- diversity_analysis |>
  #       mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
  #       filter(diversity_index == "diversity",
  #              p.value <= 0.05,
  #              term != "Intercept") |>
  #       mutate(sign = paste(term, "sign", sep = "_")) |>
  #       select(diversity_index, origSiteID:grazing, term, sign)
  #
  #     # 4 colour palette (grey, green, orange, pink)
  #     four_col_palette <- c("#999999", "#009E73", "#E69F00", "#CC79A7")
  #
  #
  #     diversity <- diversity_data |>
  #       filter(diversity_index == "diversity") |>
  #       #distinct(origSiteID, warming, grazing) |>
  #       left_join(sign, by = c("origSiteID", "grazing")) |>
  #       mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
  #                               term == "N" & warming == "warming" ~ NA_character_,
  #                               TRUE ~ sign),
  #              sign = if_else(is.na(sign),  "no effect", sign),
  #              sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming", NxW_sign = "Interaction"),
  #              sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming", "Interaction"))) |>
  #       ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
  #       geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
  #       geom_point(size = 2) +
  #       labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
  #            y = "Change in diversity") +
  #       scale_colour_manual(name = "Effect", values = four_col_palette) +
  #       scale_fill_manual(name = "Effect", values = four_col_palette) +
  #       scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
  #       scale_shape_manual(name = "warming", values = c(16, 1)) +
  #       facet_grid(origSiteID ~ grazing) +
  #       theme_minimal() +
  #       theme(text = element_text(size = 16))
  #
  #     ggsave(diversity, filename = "output/diversity.jpg", dpi = 300, width = 8, height = 6, bg = "white")
  #   }),
  #
  # # evenness
  # tar_target(
  #   name = evenness_plot,
  #   command = {
  #
  #     sign <- diversity_analysis |>
  #       mutate(term = recode(term, "(Intercept)" = "Intercept", Nitrogen_log = "N", warmingwarming = "W", "Nitrogen_log:warmingwarming" = "NxW")) |>
  #       filter(diversity_index == "evenness",
  #              p.value <= 0.05,
  #              term != "Intercept") |>
  #       mutate(sign = paste(term, "sign", sep = "_")) |>
  #       select(diversity_index, origSiteID:grazing, term, sign)
  #
  #     # 4 colour palette (grey, green)
  #     two_col_palette <- c("#999999", "#009E73")
  #
  #
  #     evenness <- diversity_data |>
  #       filter(diversity_index == "evenness") |>
  #       #distinct(origSiteID, warming, grazing) |>
  #       left_join(sign, by = c("origSiteID", "grazing")) |>
  #       mutate(sign = case_when(term == "W" & warming == "ambient" ~ NA_character_,
  #                               term == "N" & warming == "warming" ~ NA_character_,
  #                               TRUE ~ sign),
  #              sign = if_else(is.na(sign),  "no effect", sign),
  #              sign = recode(sign, N_sign = "Nitrogen", W_sign = "Warming", NxW_sign = "Interaction"),
  #              sign = factor(sign, levels = c("no effect", "Nitrogen", "Warming", "Interaction"))) |>
  #       ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
  #       geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
  #       geom_point(size = 2) +
  #       labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
  #            y = "Change in evenness") +
  #       scale_colour_manual(name = "Effect", values = two_col_palette) +
  #       scale_fill_manual(name = "Effect", values = two_col_palette) +
  #       scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
  #       scale_shape_manual(name = "warming", values = c(16, 1)) +
  #       facet_grid(origSiteID ~ grazing) +
  #       theme_minimal() +
  #       theme(text = element_text(size = 16))
  #
  #     ggsave(evenness, filename = "output/evenness.jpg", dpi = 300, width = 8, height = 6, bg = "white")
  #   }),
  #
  #


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



