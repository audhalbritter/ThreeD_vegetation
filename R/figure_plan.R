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
               Site = factor(Site, levels = c("High alpine", "Alpine", "Lowland")),
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
      richness_data %>%
        ggplot(aes(x = Namount_kg_ha_y, y = delta, colour = warming, linetype = warming)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = bquote(Nitrogen~(kg~ha^-1~y^-1)),
             y = "Difference in richness (2021 - 2019)") +
        scale_colour_manual(name = "warming", values = c("light blue", "orange")) +
        scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
        facet_grid(origSiteID ~ grazing)
  }),

  # cover
  tar_target(
    name = cover_plot,
    command = {
      cover_data %>%
        ggplot(aes(x = Namount_kg_ha_y, y = delta, colour = functional_group, linetype = warming)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = bquote(Nitrogen~(kg~ha^-1~y^-1)),
             y = "Difference in cover (2021 - 2019)") +
        scale_colour_manual(name = "", values = c("plum4", "limegreen")) +
        scale_linetype_manual(name = "warming", values = c("solid", "dashed")) +
        facet_grid(origSiteID ~ grazing)
    })

  )
