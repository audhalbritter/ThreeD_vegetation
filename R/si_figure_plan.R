si_figure_plan <- list(
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

  # treatments
  tar_target(
    name = climate_treatment_figure,
    command = make_climate_treatment_figure(daily_temp)
  ),

  ### BIOMASS AND PRODUCTIVTY
  tar_target(
    name = biomass_fun_group,
    command = make_functional_group_biomass_figure(biomass)
  ),

  # not sure if this one is needed
  tar_target(
    name = productivity_figure_boxplot,
    command = make_productivity_figure(biomass)
  ),

  # control vs cage biomass
  tar_target(
    name = cage_control_biomass_figure,
    command = productivity_raw |>
      mutate(siteID = recode(siteID, "Vik" = "lowland", "Joa" = "sub-alpine", "Lia" = "alpine"),
             siteID = factor(siteID, levels = c("lowland", "sub-alpine", "alpine")),
             treatment = factor(treatment, levels = c("Control", "Cage")),
             doy = yday(date)) |>
      group_by(date, siteID, treatment) |>
      summarise(se = sd(productivity)/sqrt(n()),
                mean = mean(productivity)) |>
      ggplot(aes(x = date, y = mean, colour = treatment, shape = treatment)) +
      geom_point(position = position_jitter(0.6)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_jitter(0.6)) +
      scale_colour_manual(values = c("#999999", "#009E73")) +
      scale_shape_manual(values = c(1, 16)) +
      labs(y = "Biomass in g",
           x = "") +
      facet_wrap(~ siteID, scales = "free_y") +
      theme_bw()
  ),

  # Height
  # tar_target(
  #   name = height_figure,
  #   command = make_vegetation_figure(dat = height_model_output,
  #                                    yaxislabel = "Change in height",
  #                                    colourpalette = NxW_col_palette,
  #                                    linetypepalette = c("solid", "dashed", "dotted"),
  #                                    shapepalette = c(16, 0, 2),
  #                                    facet_2 = "vegetation_layer") +
  #     facet_wrap(origSiteID ~ vegetation_layer, scales = "free_y")
  # ),

  # Functional group cover - natural
  tar_target(
    name = cover_CN_figure,
    command = {

      cover_CN_text <- cover_CN_stats  |>
        filter(functional_group != "sedge") |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, functional_group, term) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))

      make_vegetation_figure(dat = cover_CN_prediction |>
                               filter(functional_group != "sedge"),
                             x_axis = Nitrogen_log,
                             yaxislabel = "Change in cover",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed"),
                             shapepalette = c(16, 0),
                             #facet_1 = "origSiteID",
                             facet_2 = "functional_group") +
        # add stats
        geom_text(data = cover_CN_prediction |>
                    filter(functional_group != "sedge") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_CN_text, by = c("origSiteID", "functional_group")) |>
                    mutate(term = if_else(is.na(term), "", term)),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.4, vjust = 1.4)


    }

        ),

  # diversity figure
  tar_target(
    name = diversity_CN_figure,
    command = {

      diversity_CN_text <- diversity_CN_stats |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        ### BY HAND!!!
        filter(significance == "sign",
               term != "WxN") |>
        distinct(origSiteID, diversity_index, term) |>
        mutate(term = if_else(term == "WxGxN", "WxN + WxGxN", term)) |>
        filter(!(origSiteID == "Alpine" & diversity_index == "diversity" & term == "WxN")) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))


      make_vegetation_figure(dat = diversity_CN_prediction,
                             x_axis = Nitrogen_log,
                             yaxislabel = "Change in diversity index",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed"),
                             shapepalette = c(16, 0),
                             facet_2 = "diversity_index")  +
        facet_grid2(origSiteID ~ diversity_index, scales = "free_y", independent = "y") +
        # add stats
        geom_text(data = diversity_CN_prediction |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(diversity_CN_text, by = c("origSiteID", "diversity_index")),
                  aes(x = Inf, y = Inf, label = term),
                  size = 4, colour = text_colour, hjust = 1.2, vjust = 1.4)

    }
  )

)
