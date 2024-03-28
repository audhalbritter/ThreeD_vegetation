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

      climate_text <- climate_anova_table |>
        mutate(significance = case_when(p.value >= 0.05 ~ "non-sign",
                                        term == "Residuals" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign")


      clim <- climate_output |>
        unnest(data) |>
        mutate(variable = factor(variable, levels = c("air", "ground", "soil", "soilmoisture")))

      temp <- make_climate_figure(dat1 = clim |>
                            filter(variable != "soilmoisture"),
                             x_axis = Nitrogen_log,
                             yaxislabel = "Temperature in °C",
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed", "dotted"),
                             shapepalette = c(16, 0, 2),
                             facet_2 = "variable",
                             dat2 = climate_prediction |>
                            filter(variable != "soilmoisture")) +
        labs(tags = "a)") +
        # add stats
        geom_text(data = clim |>
                    filter(variable != "soilmoisture") |>
                    distinct(origSiteID, variable, warming, Namount_kg_ha_y, grazing) |>
                    left_join(climate_text |>
                                filter(variable != "soilmoisture"), by = c("origSiteID", "variable")),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour)

      moisture <- make_climate_figure(dat1 = clim |>
                            filter(variable == "soilmoisture"),
                          x_axis = Nitrogen_log,
                          yaxislabel = "Soilmoisture in %",
                          colourpalette = col_palette,
                          linetypepalette = c("solid", "dashed", "dotted"),
                          shapepalette = c(16, 0, 2),
                          facet_2 = "variable",
                          dat2 = climate_prediction |>
                            filter(variable == "soilmoisture")) +
        labs(tags = "b)") +
        # add stats
        geom_text(data = clim |>
                    filter(variable == "soilmoisture") |>
                    distinct(origSiteID, variable, warming, Namount_kg_ha_y, grazing) |>
                    left_join(climate_text |>
                                filter(variable == "soilmoisture"), by = c("origSiteID", "variable")),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour)

      temp + moisture + plot_layout(guides = "collect", widths = c(3, 1)) &
        theme(legend.position = "top")
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
    name = productivity_consumption_figure,
    command = {

      dat <- productivity_raw |>
        # calculate productivity in mg per m^2
        mutate(productivity_mg_m2 = productivity / area_cm / 10000 * 1000) |>
        # productivity per day
        mutate(productivity_mg_m2_d = productivity_mg_m2 / duration) |>
        mutate(siteID = recode(siteID, "Vik" = "Lowland", "Joa" = "Sub-alpine", "Lia" = "Alpine"),
               siteID = factor(siteID, levels = c("Lowland", "Sub-alpine", "Alpine")),
               treatment = case_match(treatment,
                                      "Control" ~ "Grazed",
                                      "Cage" ~ "Ungrazed"),
               treatment = factor(treatment, levels = c("Grazed", "Ungrazed")))

      # avaerage duration
      dat |>
        summarise(se = sd(duration, na.rm = TRUE)/sqrt(n()),
                  mean = mean(duration, na.rm = TRUE))

      # # Daily productivity over the growing season
        # plot1 <- dat |>
        #   group_by(date_out, siteID, treatment) |>
        #   summarise(se = sd(productivity_mg_m2_d)/sqrt(n()),
        #             mean = mean(productivity_mg_m2_d)) |>
        #   ggplot(aes(x = date_out, y = mean, colour = treatment)) +
        #   geom_point(position = position_jitter(0.9)) +
        #   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_jitter(0.9)) +
        #   scale_colour_manual(name = "", values = c("#E9BD7F", "grey30")) +
        #   labs(y = bquote(Biomass~g~m^-2),
        #        x = "",
        #        tag = "a)") +
        #   facet_wrap(~ siteID) +
        #   theme_bw()

        # annual productivity
        dat |>
          summarise(sum = sum(productivity_mg_m2),
                    .by = c(siteID, treatment, plot_nr)) |>
          summarise(se = sd(sum)/sqrt(n()),
                    mean = mean(sum),
                    .by = c(siteID, treatment)) |>
          select(siteID, treatment, mean, se)

        # Annual productivity in grazed and ungrazed plots
        plot2 <- dat |>
          # remove first round without control
          filter(!is.na(date_in)) |>
          group_by(siteID, treatment, plot_nr) |>
          summarise(sum = sum(productivity_mg_m2)) |>
          ggplot(aes(x = siteID, y = sum, fill = treatment)) +
          geom_boxplot() +
          scale_fill_manual(name = "", values = c("#E9BD7F", "grey30")) +
          labs(y = bquote(Annual~productivity~mg~m^-2~y^-1),
               x = "",
               tag = "a)") +
          theme_bw()


        # Seasonal biomass consumption
        dat2 <- dat |>
          # remove first round without control
          filter(!is.na(date_in)) |>
          select(-productivity, -productivity_mg_m2) |>
          pivot_wider(names_from = treatment, values_from = productivity_mg_m2_d) |>
          mutate(Consumption = Ungrazed - Grazed)

        dat2 |>
          summarise(sum = sum(Consumption),
                    .by = c(siteID, plot_nr)) |>
          summarise(se = sd(sum)/sqrt(n()),
                    mean = mean(sum),
                    .by = c(siteID)) |>
          select(siteID, mean, se)


      # plot3 <- dat2 |>
      #   ggplot(aes(x = date, y = Consumption)) +
      #   geom_hline(yintercept = 0, colour = "grey70") +
      #   geom_point(colour = "#E9BD7F") +
      #   labs(y = bquote(Biomass~consumption~g~m^-2),
      #        x = "",
      #        tag = "c)",
      #        title = "Seasonal biomass consumption") +
      #   facet_wrap(~ siteID) +
      #   theme_bw()

      # Annual biomass consumption
      plot4 <- dat |>
        select(-productivity, -productivity_mg_m2_d) |>
        group_by(siteID, treatment, plot_nr) |>
        summarise(sum = sum(productivity_mg_m2)) |>
        pivot_wider(names_from = treatment, values_from = sum) |>
        mutate(Consumption = Ungrazed - Grazed) |>
        ggplot(aes(x = siteID, y = Consumption)) +
        geom_hline(yintercept = 0, colour = "grey70") +
        geom_boxplot(fill = "#E9BD7F") +
        labs(y = bquote(Annual~biomass~consumption~mg~m^-2~y-1),
             x = "",
             tag = "b)") +
        theme_bw()

      (plot2 + plot4) + plot_layout(guides = "collect") &
        theme(text = element_text(size = 12))

    }


  ),


  ### PRODUCTIVTY
  # productivity and cover figure in one
  tar_target(
    name = productivity_figure,
    command = {

      productivity_text <- productivity_anova_table |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "G", "WxN"))) |>
        group_by(origSiteID, significance)

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
        labs(x = "") +
        # add stats
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text |>
                                filter(significance == "sign") |>
                                slice(1), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text |>
                                filter(significance == "sign") |>
                                slice(2), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 3, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text |>
                                filter(significance == "sign") |>
                                slice(3), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 4.6, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text |>
                                filter(significance == "sign") |>
                                slice(4), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 6.2, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = productivity_prediction |>
                    distinct(origSiteID, warming, Namount_kg_ha_y, grazing) |>
                    left_join(productivity_text |>
                                filter(significance == "marginal") |>
                                slice(1), by = "origSiteID"),
                  aes(x = -Inf, y = Inf, hjust = 0, vjust = 7.8, label = term),
                  size = 3, colour = text_colour)

      }
    ),


  ### FUNCTIONAL GROUP COVER - NATURAL GRAZING
  tar_target(
    name = cover_CN_figure,
    command = {

      cover_CN_text <- cover_CN_anova_table |>
        mutate(significance = case_when(is.na(p.value) ~ "non-sign",
                                        p.value > 0.07 ~ "non-sign",
                                        p.value > 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        filter(significance %in% c("sign", "marginal")) |>
        select(origSiteID, functional_group, significance, term) |>
        mutate(x_pos = if_else(functional_group == "legume", Inf, -Inf),
               y_pos = if_else(functional_group ==  "graminoid", -Inf, Inf),
               hjust_var = if_else(functional_group ==  "legume", 1, 0),
               vjust_var = if_else(functional_group ==  "graminoid", 0, 1)) |>
        mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))

      sign_text <- cover_CN_text |>
        filter(significance == "sign")

      marg_text <- cover_CN_text |>
        filter(significance == "marginal")

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
        # forbs
        geom_text(data = cover |>
                    filter(functional_group %in% c("forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group %in% c("forb")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(1), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 1.4, label = term),
                  size = 3, colour = text_colour) +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("forb")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marg_text |>
                                filter(functional_group %in% c("forb")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(1), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 1.4, label = term),
                  size = 3, colour = "grey50") +
        # graminoid
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group %in% c("graminoid")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(1), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = -4.4, label = term),
                  size = 3, colour = text_colour) +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group %in% c("graminoid")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(2), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = -2.8, label = term),
                  size = 3, colour = text_colour) +
        # add stats
        geom_text(data = cover |>
                    filter(functional_group %in% c("graminoid")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group %in% c("graminoid")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(3), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = -1.2, label = term),
                  size = 3, colour = text_colour)


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
        theme(axis.text.x=element_blank()) +
        geom_text(data = cover |>
                    filter(functional_group %in% c("sedge")) |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(marg_text |>
                                filter(functional_group %in% c("sedge")) |>
                                group_by(origSiteID, functional_group) |>
                                slice(1), by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 1.4, label = term),
                  size = 3, colour = "grey50")

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
                    left_join(sign_text |>
                                filter(functional_group == "legume") |>
                                group_by(origSiteID, functional_group) |>
                                slice(5),
                              by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 7.6, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group == "legume") |>
                                group_by(origSiteID, functional_group) |>
                                slice(4),
                              by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 6, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group == "legume") |>
                                group_by(origSiteID, functional_group) |>
                                slice(3),
                              by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 4.4, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group == "legume") |>
                                group_by(origSiteID, functional_group) |>
                                slice(2),
                              by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 2.8, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = cover |>
                    filter(functional_group == "legume") |>
                    distinct(origSiteID, functional_group, warming, Namount_kg_ha_y, grazing) |>
                    left_join(sign_text |>
                                filter(functional_group == "legume") |>
                                group_by(origSiteID, functional_group) |>
                                slice(1),
                              by = c("origSiteID", "functional_group")),
                  aes(x = x_pos, y = y_pos, hjust = hjust_var, vjust = 1.2, label = term),
                  size = 3, colour = text_colour)


      # merge figures
      cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(2, 1)) &
        theme(legend.position = "top")

      cover_figure + sedge_figure/legumes_figure +
        plot_layout(guides = "collect", widths = c(2, 1)) &
        theme(legend.position = "top")


    }

        ),

  ### DIVERSITY CN FIGURE
  tar_target(
    name = diversity_CN_figure,
    command = {

      diversity_CN_text <- diversity_CN_anova_table |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value > 0.07 ~ "non-sign",
                                        p.value > 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, diversity_index, term, significance) |>
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

      div_sign <- diversity_CN_text |>
        filter(significance == "sign")


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
                    left_join(div_sign |>
                                group_by(origSiteID, diversity_index) |>
                                slice(1), by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -5.3, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = div |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(div_sign |>
                                group_by(origSiteID, diversity_index) |>
                                slice(2), by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -3.9, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = div |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(div_sign |>
                                group_by(origSiteID, diversity_index) |>
                                slice(3), by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -2.5, label = term),
                  size = 3, colour = text_colour) +
        geom_text(data = div |>
                    distinct(origSiteID, diversity_index, warming, Namount_kg_ha_y, grazing) |>
                    left_join(div_sign |>
                                group_by(origSiteID, diversity_index) |>
                                slice(4), by = c("origSiteID", "diversity_index")),
                  aes(x = -Inf, y = -Inf, hjust = 0, vjust = -1.1, label = term),
                  size = 3, colour = text_colour)


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
