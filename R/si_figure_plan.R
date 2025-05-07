si_figure_plan <- list(

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

  ### CLIMATE
  tar_target(
    name = daily_climate_figure,
    command = make_daily_climate_figure(daily_temp, col_palette)
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


  # needed?
  tar_target(
    name = biomass_fun_group,
    command = make_functional_group_biomass_figure(biomass)
  ),

  # control vs cage biomass
  tar_target(
    name = annual_productivity,
    command = productivity_raw |>
      # calculate productivity in g per m^2
      mutate(productivity_g_m2 = productivity * 10000 / area_cm) |>
      # productivity per day
      mutate(productivity_g_m2_d = productivity_g_m2 / duration) |>
      mutate(siteID = recode(siteID, "Vik" = "Lowland", "Joa" = "Sub-alpine", "Lia" = "Alpine"),
             siteID = factor(siteID, levels = c("Lowland", "Sub-alpine", "Alpine")),
             treatment = case_match(treatment,
                                    "Control" ~ "Grazed",
                                    "Cage" ~ "Ungrazed"),
             treatment = factor(treatment, levels = c("Grazed", "Ungrazed")))
      ),

  # Biomass consumption
  tar_target(
    name = consumption,
    command = annual_productivity |>
      select(-productivity, -productivity_g_m2_d) |>
      group_by(siteID, treatment, plot_nr) |>
      summarise(sum = sum(productivity_g_m2)) |>
      pivot_wider(names_from = treatment, values_from = sum) |>
      mutate(Consumption = Ungrazed - Grazed)
    ),


  # control vs cage biomass
  tar_target(
    name = productivity_consumption_figure,
    command = {

      # average duration
      annual_productivity |>
        summarise(se = sd(duration, na.rm = TRUE)/sqrt(n()),
                  mean = mean(duration, na.rm = TRUE))

        # Annual productivity in grazed and ungrazed plots
        plot2 <- annual_productivity |>
          # remove first round without control
          filter(!is.na(date_in)) |>
          group_by(siteID, treatment, plot_nr) |>
          summarise(sum = sum(productivity_g_m2)) |>
          ggplot(aes(x = siteID, y = sum, fill = treatment)) +
          geom_boxplot() +
          scale_fill_manual(name = "", values = c("#E9BD7F", "grey30")) +
          labs(y = bquote(Annual~productivity~g~m^-2~y^-1),
               x = "",
               tag = "a)") +
          theme_bw()

      # Annual biomass consumption
      plot4 <- consumption |>
        ggplot(aes(x = siteID, y = Consumption)) +
        geom_hline(yintercept = 0, colour = "grey70") +
        geom_boxplot(fill = "#E9BD7F") +
        labs(y = bquote(Annual~biomass~consumption~g~m^-2~y-1),
             x = "",
             tag = "b)") +
        theme_bw()

      (plot2 + plot4) + plot_layout(guides = "collect") &
        theme(text = element_text(size = 12),
              legend.position = "top")

    }
  ),

  tar_target(
    name = standing_biomass_back_fig,
    command = {

      r.squared <- round(standing_biomass_model_output$r.squared, 2)
      f.stat <- standing_biomass_model_output$fstatistic
      p.val <- pf(f.stat[1], f.stat[2], f.stat[3], lower.tail=FALSE)
      p.val.round <- if_else(p.val < 0.001, "<0.001", paste0("= ", as.character(round(p.val, 3))))

      dat <- prep_SB_back |>
        filter(year == 2022)

      new_data <- crossing(dat |>
                             ungroup() |>
                             select(biomass_remaining_calc),
                           tibble(Nitrogen_log = c(0, 4.62)))

      prediction <- augment(SB_back_model_22, newdata = new_data)

      ggplot(dat,
             aes(x = biomass_remaining_calc, y = biomass_remaining_coll)) +
        geom_line(data = prediction,
                  aes(y = .fitted, group = Nitrogen_log, linetype = as.factor(Nitrogen_log)),
                  colour = "grey60") +
        geom_point(aes(colour = warming, size = Namount_kg_ha_y)) +
        annotate("text", x = 2000, y = 5,
                 label = bquote(R^2 == .(r.squared) ~ ", P" ~ .(p.val.round))) +
        scale_colour_manual(values = col_palette, name = "Warming") +
        scale_size_continuous(name = bquote(Nitrogen~addition~(kg~ha^-1~y^-1))) +
        guides(linetype = FALSE) +
        labs(x = "Cover x height",
             y = bquote(Estimated~standing~biomass~(g~m^-2))) +
        theme_bw()

    }
  ),

  # Biomass vs. diversity analysis
  tar_target(
    name = standingB_div_final_model,
    command = lm(final_diversity ~ log(final_bio) * origSiteID * warming, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_final_prediction,
    command = augment(standingB_div_final_model)
  ),

  tar_target(
    name = standingB_div_change_model,
    command = lm(log_ratio_diversity ~ log_ratio_bio * origSiteID * warming, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_change_prediction,
    command = augment(standingB_div_change_model)
  ),

  # Biomass vs diversity figure
  tar_target(
    name = standingB_div_change_figure,
    command = {

      biomass_div |>
        ggplot(aes(x = log_ratio_bio, y = log_ratio_diversity)) +
        geom_line(data = standingB_div_change_prediction, 
          aes(y = .fitted,
            x = log_ratio_bio,
            linetype = origSiteID,
            colour = warming),
            linewidth = 0.75) +
        geom_point(data = biomass_div, aes(colour = warming,
                                           shape = grazing,
                                           fill = interaction(origSiteID, warming),
                                           size = Namount_kg_ha_y)) +
        scale_colour_manual(values = col_palette, name = "Warming") +
        # Fill colors: White for Alpine, specific colors for Sub-Alpine (warming levels)
        scale_fill_manual(values = c("grey30", "white", "#FD6467", "white"),
                          name = "Origin",
                          #breaks = c("Alpine", "Sub-alpine"),
                          labels = c("Alpine", "Sub-Alpine", "", ""),
                          guide = guide_legend(override.aes = list(
                            shape = 21,
                            colour = "grey30",
                            fill = c("white", "grey30")
                          ))) +
        scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +
        scale_size_continuous(name = "Nitrogen") +
        scale_linetype_manual(values = c("dashed", "solid"),
                              name = "Origin") +
        labs(x = bquote(Log(Change~standing~biomass)~g~m^-2),
             y = "Change in Shannon diversity") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.box = "vertical",
              text = element_text(size = 12))

    }
  )


  # tar_target(
  #   name = biomass_consumption_figure,
  #   command = {
  #
  #     b <- biomass_div |>
  #       filter(warming == "Ambient", Namount_kg_ha_y == 0) |>
  #       ggplot(aes(x = grazing, y = biomass_remaining_calc)) +
  #       geom_boxplot() +
  #       labs(x = "",
  #            y = "Estimated standing biomass (cover x height)") +
  #       facet_wrap(~ origSiteID) +
  #       theme_bw()
  #
  #     c <- biomass_div |>
  #       filter(warming == "Ambient", Namount_kg_ha_y == 0) |>
  #       ggplot(aes(x = grazing, y = consumption)) +
  #       geom_boxplot() +
  #       labs(x = "",
  #            y = "Estimated consumption") +
  #       facet_wrap(~ origSiteID) +
  #       theme_bw()
  #
  #     b/c
  #
  #   }
  #
  #   ),
  #
  # tar_target(
  #   name = biomass_calc_coll_figure,
  #   command = {
  #
  #     dat <- biomass_div |>
  #       filter(grazing != "Natural")
  #
  #       dat$pred <- predict(biomass_calc_coll_model, newdata = dat)
  #
  #     ggplot(dat, aes(x = log(biomass_remaining_coll + 10), y = log(biomass_remaining_calc),
  #                             linetype = grazing)) +
  #       geom_line(aes(x = log(biomass_remaining_coll + 10), y = pred), colour = "grey40") +
  #       geom_point(aes(colour = warming, shape = grazing, size = Namount_kg_ha_y)) +
  #       scale_colour_manual(values = col_palette) +
  #       scale_shape_manual(values = c(16, 0, 2, 5)) +
  #       scale_size_continuous(name = "Nitrogen") +
  #       labs(x = "Log(Standing biomass collected) (g/m2)",
  #            y = "Log(Estimated standing biomass) (cover x height)") +
  #       theme_bw()
  #
  #   }
  #
  # )

)


## productivity after clipping or grazing
# # prod clipping
# productivity |>
#   filter(warming == "Ambient", Namount_kg_ha_y == 0) |>
#   group_by(origSiteID, grazing) |>
#   summarise(mean(productivity))
#
# # prod grazing
# annual_productivity |>
#   filter(treatment == "Ungrazed") |>
#   group_by(siteID, date_out) |>
#   summarise(prod = mean(productivity_g_m2)) |>
#   group_by(siteID) |>
#   summarise(prod = sum(prod),
#             n = n())
