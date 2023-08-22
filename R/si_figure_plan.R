si_figure_plan <- list(

  ### CLIMATE
  # annual climate figure
  tar_target(
    name = annual_climate_figure,
    command = make_annual_climate_figure(annual_climate)
  ),

  tar_target(
    name = daily_climate_figure,
    command = make_daily_climate_figure(daily_temp, col_palette)
  ),

  # treatments
  tar_target(
    name = climate_treatment_figure,
    command = make_climate_treatment_figure(daily_temp)
  ),

  # climate figure
  tar_target(
    name = climate_figure,
    command = {

      climate_text <- climate_stats |>
        mutate(significance = case_when(p.value >= 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign")


      clim <- climate_output |>
        unnest(data) |>
        mutate(variable = factor(variable, levels = c("air", "ground", "soil", "soilmoisture")))

      make_climate_figure(dat1 = clim,
                             x_axis = Nitrogen_log,
                             yaxislabel = "Climate variable",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed", "dotted"),
                             shapepalette = c(16, 0, 2),
                             facet_2 = "variable",
                             dat2 = climate_prediction)  +
        facet_grid2(origSiteID ~ variable, scales = "free_y", independent = "y") +
        # add stats
        geom_text(data = clim |>
                    distinct(origSiteID, variable, warming, Namount_kg_ha_y, grazing) |>
                    left_join(climate_text, by = c("origSiteID", "variable")),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour)

    }
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
      group_by(date, doy, siteID, treatment) |>
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



  # Functional group cover - natural
  tar_target(
    name = cover_CN_figure,
    command = {

      cover_CN_text <- cover_CN_stats  |>
        mutate(significance = case_when(p.value > 0.05 ~ "non-sign",
                                        term == "Intercept" ~ "non-sign",
                                        names == "quadratic" & str_sub(term, -1, -1) == "N" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        ### BY HAND CODE!!!
        group_by(origSiteID, functional_group) |>
        slice(1) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume"))) |>
        select(origSiteID, functional_group, term)

      cover_CN_text2 <- cover_CN_text |>
        mutate(term = case_when(term == "WxN" ~ "WxGxN",
                                term == "G" ~ "GxN",
                                TRUE ~ ""),
               functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))

      marginal_CN_cover_text <- cover_CN_text |>
        mutate(term = case_when(term == "G" ~ "WxG",
                                TRUE ~ "")) |>
        bind_rows(tibble(origSiteID = "Sub-alpine",
                         functional_group = "graminoid",
                         term = "W")) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))

      cover = cover_CN_output |>
        unnest(data) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume"))) |>
        mutate(grazing = .grazing)

      cover_figure <- make_vegetation_figure(dat1 = cover  |>
                                               filter(functional_group %in% c("graminoid", "forb")),
                             x_axis = Nitrogen_log,
                             yaxislabel = "Change in cover",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed"),
                             shapepalette = c(16, 0),
                             facet_2 = "functional_group",
                             dat2 = cover_CN_prediction |>
                               filter(functional_group %in% c("graminoid", "forb"))) +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_CN_text |>
                                filter(functional_group %in% c("graminoid", "forb")), by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 1.2, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_CN_text2 |>
                                filter(functional_group %in% c("graminoid", "forb")), by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 2.8, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid", "forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marginal_CN_cover_text |>
                                filter(functional_group %in% c("graminoid", "forb")), by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 1.2, label = term),
                  size = 3, colour = "grey50")


      sedge_figure <- make_vegetation_figure(dat1 = cover |>
                                               filter(functional_group == "sedge"),
                                             x_axis = Nitrogen_log,
                                             yaxislabel = "Change in cover",
                                             colourpalette = col_palette,
                                             linetypepalette = c("solid", "dashed", "dotted"),
                                             shapepalette = c(16, 0, 2),
                                             facet_2 = "functional_group",
                                             dat2 = cover_CN_prediction |>
                                               filter(functional_group == "sedge")) +
        labs(x = "", y = "") +
        theme(axis.text.x=element_blank())

      legumes_figure <- make_vegetation_figure(dat1 = cover |>
                                                 filter(functional_group == "legume"),
                                               x_axis = Nitrogen_log,
                                               yaxislabel = "Change in cover",
                                               colourpalette = col_palette,
                                               linetypepalette = c("solid", "dashed", "dotted"),
                                               shapepalette = c(16, 0, 2),
                                               facet_2 = "functional_group",
                                               dat2 = cover_CN_prediction |>
                                                 filter(functional_group == "legume")) +
        labs(x = "", y = "") +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_CN_text |>
                                filter(functional_group == "legume")
                              , by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 1.2, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(cover_CN_text2 |>
                                filter(functional_group == "legume")
                              , by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 2.8, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marginal_CN_cover_text |>
                                filter(functional_group == "legume"),
                  by = c("origSiteID", "functional_group")),
                  aes(x = Inf, y = Inf, hjust = 1, vjust = 4.4, label = term),
                  size = 3, colour = "grey50")

      # merge figures
      cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(2, 1)) &
        theme(legend.position = "top")

      cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(2, 1)) &
        theme(legend.position = "top")


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
        filter(significance == "sign") |>
        distinct(origSiteID, diversity_index, term) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

      marginal_div_CN_text <- diversity_CN_text |>
      mutate(term = case_when(origSiteID == "Alpine" & diversity_index == "diversity" ~ "WxGxN",
                              TRUE ~ "")) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))


      div <- diversity_CN_output |>
        unnest(data) |>
        mutate(grazing = .grazing) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

      make_vegetation_figure(dat1 = div,
                             x_axis = Nitrogen_log,
                             yaxislabel = "Change in diversity index",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed"),
                             shapepalette = c(16, 0),
                             facet_2 = "diversity_index",
                             dat2 = diversity_CN_prediction)  +
        facet_grid2(origSiteID ~ diversity_index, scales = "free_y", independent = "y") +
        # add stats
        geom_text(data = div |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(diversity_CN_text, by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -2.5, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = div |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marginal_div_CN_text, by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -1.1, label = term),
                  size = 3, colour = "grey50")

    }
  )

)

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
