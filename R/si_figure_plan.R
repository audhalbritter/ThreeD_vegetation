si_figure_plan <- list(

  # wes anderson colour palette
  # tar_target(
  #   name = col_palette,
  #   #command = wes_palette("GrandBudapest1")[c(1, 2)]
  #   command = c("grey30", "#FD6467")
  # ),

  # Gauguin colour palette
  tar_target(
    name = treatment_palette,
    # colours for treatments: 1 = black (control), 2 = pink (warming), 3 = yellow (clipping), 4 = green (nitrogen), 5 = blue (biomass) ("#2f70a1", "#0a3351")
    command = c("grey20", MetBrewer::met.brewer(name="Gauguin", n=3, type="discrete"), "#2f70a1")
  ),

  tar_target(
    name = warming_palette,
    command = treatment_palette[c(1, 2)]
  ),

  tar_target(
    name = biomass_palette,
    command = c(MetBrewer::met.brewer(name="Hokusai2", n=5, type="discrete"))
  ),

  tar_target(
    name = nitrogen_palette,
    command = c(MetBrewer::met.brewer(name="VanGogh3", n=7, type="discrete"))
  ),

    tar_target(
    name = grazing_palette,
    command = c(treatment_palette[c(1, 3)], "tan4")
  ),

  tar_target(
    name = text_colour,
    command = wes_palette("IsleofDogs2")[4]
  ),

  ### CLIMATE
  tar_target(
    name = daily_climate_figure,
    command = make_daily_climate_figure(daily_temp, col_palette = warming_palette)
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
                                  colourpalette = warming_palette,
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
                                      colourpalette = warming_palette,
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
      mutate(productivity_g_m2 = productivity * 10000 / area_cm2) |>
      # productivity per day
      mutate(productivity_g_m2_d = productivity_g_m2 / duration) |>
      mutate(siteID = recode(destSiteID, "Vikesland" = "Lowland", "Joasete" = "Sub-alpine", "Liahovden" = "Alpine"),
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
          geom_violin(draw_quantiles = c(0.5)) +
          scale_fill_manual(name = "", values = treatment_palette[c(3, 1)]) +
          labs(y = bquote(Annual~productivity~g~m^-2~y^-1),
               x = "",
               tag = "a)") +
          theme_bw()

      # Annual biomass consumption
      plot4 <- consumption |>
        ggplot(aes(x = siteID, y = Consumption)) +
        geom_hline(yintercept = 0, colour = "grey70") +
        geom_violin(fill = treatment_palette[c(3)], draw_quantiles = c(0.5)) +
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
                           tibble(Nitrogen_log = c(0, log(100 + 1))))

      prediction <- augment(SB_back_model_22, newdata = new_data)

      ggplot(dat,
             aes(x = biomass_remaining_calc, y = biomass_remaining_coll)) +
        geom_line(data = prediction,
                  aes(y = .fitted, group = Nitrogen_log, linetype = as.factor(Nitrogen_log)),
                  colour = "grey60") +
        geom_point(aes(colour = warming, size = Nitrogen_log)) +
        annotate("text", x = 2000, y = 5,
                 label = as.expression(bquote(R^2 == .(r.squared) ~ ", P" ~ .(p.val.round)))) +
        scale_colour_manual(values = warming_palette, name = "Warming") +
        scale_size_continuous(name = bquote(Log(Nitrogen)~kg~ha^-1~y^-1),
                             breaks = c(0, 1, 2, 3, 4),
                             labels = c("0", "25", "50", "75", "100")) +
        guides(linetype = "none") +
        labs(x = "Cover x height",
             y = bquote(Estimated~standing~biomass~(g~m^-2))) +
        theme_bw()

    }
  ),

  # Biomass vs. diversity analysis
  tar_target(
    name = standingB_div_final_model,
    command = biomass_div |>
      group_by(origSiteID) |>
      nest() |>
      mutate(
        model = map(data, ~lm(final_diversity ~ log(final_bio), data = .x))
      )
  ),

  tar_target(
    name = standingB_div_final_prediction,
    command = standingB_div_final_model |>
      mutate(
        predictions = map2(model, data, ~augment(.x, newdata = .y))
      ) |>
      select(origSiteID, predictions) |>
      unnest(predictions) 
  ),

  tar_target(
    name = standingB_div_final_result,
    command = standingB_div_final_model |>
      mutate(
        result = map(model, tidy),
        anova = map(model, car::Anova),
        anova_tidy = map(anova, tidy)) |>
      select(origSiteID, anova_tidy) |>
      unnest(anova_tidy)
  ),

### unsure if this model is needed. if yes, the need to make it the same as the final model!!!
  tar_target(
    name = standingB_div_change_model,
    command = biomass_div |>
      group_by(origSiteID) |>
      nest() |>
      mutate(
        model = map(data, ~lm(log_ratio_diversity ~ log_ratio_bio * warming, data = .x))
      )
  ),

  tar_target(
    name = standingB_div_change_prediction,
    command = standingB_div_change_model |>
      mutate(
        predictions = map2(model, data, ~augment(.x, newdata = .y))
      ) |>
      select(origSiteID, predictions) |>
      unnest(predictions)
  ),

  # Biomass vs diversity figure
  tar_target(
    name = standingB_div_change_figure,
    command = {

      biomass_div |>
        ggplot(aes(x = log_ratio_bio, y = log_ratio_diversity)) +
        # geom_line(data = standingB_div_change_prediction, 
        #   aes(y = .fitted,
        #     x = log_ratio_bio,
        #     linetype = origSiteID,
        #     colour = warming),
        #     linewidth = 0.75) +
        geom_point(data = biomass_div, aes(colour = warming,
                                           shape = grazing,
                                           fill = warming,
                                           size = Namount_kg_ha_y)) +
        scale_colour_manual(values = warming_palette, name = "Warming") +
        scale_fill_manual(values = warming_palette, name = "Warming") +
        scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +
        scale_size_continuous(name = "Nitrogen") +
        scale_linetype_manual(values = c("dashed", "solid"),
                              name = "Origin") +
        labs(x = bquote(Log(Change~standing~biomass)~g~m^-2),
             y = "Change in Shannon diversity") +
        facet_wrap(vars(origSiteID)) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.box = "vertical",
              text = element_text(size = 12))

    }
  ),


  ### Figure S5
  # richness and drivers
  tar_target(
    name = div_index_figure,
    command = {

      # variance explained labels
      richness_r2 <- diversity_origin_output |>
        filter(diversity_index == "richness") |>
        transmute(origSiteID,
                  r2_label = paste0("R² = ", round(r.squared, 2),
                                   "\nAdj R² = ", round(adj.r.squared, 2)))

      evenness_r2 <- diversity_origin_output |>
        filter(diversity_index == "evenness") |>
        transmute(origSiteID,
                  r2_label = paste0("R² = ", round(r.squared, 2),
                                   "\nAdj R² = ", round(adj.r.squared, 2)))

      ### richness by origin
      rich_text2 <- diversity_origin_anova_table |>
        filter(diversity_index == "richness") |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))

        rich <- make_vegetation_figure(dat1 = diversity_origin_output |>
                                      filter(diversity_index == "richness") |>
                                      unnest(data),
                                      x_axis = Nitrogen_log,
                                      yaxislabel = "Species richness",
                                      colourpalette = warming_palette,
                                      linetypepalette = c("solid", "dashed", "dotted"),
                                      shapepalette = c(21, 22, 24),
                                      facet_2 = NA,
                                      # predictions
                                      dat2 = diversity_origin_prediction |>
                                      filter(diversity_index == "richness")) +
        labs(tag = "a)") +
        # add stats
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "richness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(rich_text2 |>
                                group_by(origSiteID) |>
                                slice(1),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.2, vjust = -1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "richness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(rich_text2 |>
                                group_by(origSiteID) |>
                                slice(2),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.3, vjust = -3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "richness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(rich_text2 |>
                                group_by(origSiteID) |>
                                slice(3),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.05, vjust = -4.6, label = term),
                  size = 3, colour = "grey60", nudge_x = 50) +
        # add R2 labels
        geom_text(data = richness_r2,
                  inherit.aes = FALSE,
                  aes(x = Inf, y = Inf, label = r2_label, group = origSiteID),
                  hjust = 1.05, vjust = 1.2,
                  size = 3, colour = text_colour)
      

      ### evenness by origin
      even_text2 <- diversity_origin_anova_table |>
        filter(diversity_index == "evenness") |>
        mutate(significance = case_when(term == "Residuals" ~ "non-sign",
                                        p.value >= 0.07 ~ "non-sign",
                                        p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
                                        TRUE ~ "sign")) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))


      even <- make_vegetation_figure(dat1 = diversity_origin_output |>
                                      filter(diversity_index == "evenness") |>
                                      unnest(data),
                                      x_axis = Nitrogen_log,
                                      yaxislabel = "Evenness",
                                      colourpalette = warming_palette,
                                      linetypepalette = c("solid", "dashed", "dotted"),
                                      shapepalette = c(21, 22, 24),
                                      facet_2 = NA,
                                      # predictions
                                      dat2 = diversity_origin_prediction |>
                                      filter(diversity_index == "evenness")) +
        labs(tag = "b)") +
        # add stats
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "evenness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(even_text2 |>
                                group_by(origSiteID) |>
                                slice(1),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.2, vjust = -1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "evenness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(even_text2 |>
                                group_by(origSiteID) |>
                                slice(2),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.3, vjust = -3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "evenness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(even_text2 |>
                                group_by(origSiteID) |>
                                slice(3),
                              by = c("origSiteID")),
                  aes(x = -Inf, y = -Inf, hjust = -0.05, vjust = -4.6, label = term),
                  size = 3, colour = "grey60", nudge_x = 50) +
        geom_text(data = diversity_origin_prediction |>
                    filter(diversity_index == "evenness") |>
                    distinct(origSiteID, warming, Nitrogen_log, grazing) |>
                    left_join(even_text2 |>
                                  group_by(origSiteID) |>
                                  slice(4),
                              by = c("origSiteID")),
                    aes(x = -Inf, y = -Inf, hjust = -0.05, vjust = -6.2, label = term),
                    size = 3, colour = text_colour, nudge_x = 50) +
        # add R2 labels
        geom_text(data = evenness_r2,
                  inherit.aes = FALSE,
                  aes(x = Inf, y = Inf, label = r2_label, group = origSiteID),
                  hjust = 1.05, vjust = 1.2,
                  size = 3, colour = text_colour)

      (rich + even) + plot_layout(guides = "collect") &
        theme(legend.position = "top",
              plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
              legend.background = element_rect(fill = "transparent"))

    }
  )

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
