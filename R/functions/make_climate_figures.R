### make climate figures

make_climate_figure <- function(dat1,
                                   x_axis,
                                   yaxislabel,
                                   colourpalette, linetypepalette, shapepalette,
                                   facet_2,
                                   # predictions
                                   dat2){

  dat2 <- dat2 |>
    rename(.x_axis = {{x_axis}})

  plot <- dat1 |>
    rename(.x_axis = {{x_axis}}) |>
    ggplot(aes(x = .x_axis,
               y = .response,
               color = warming,
               linetype = grazing,
               shape = grazing)) +
    # CI from prediction
    geom_ribbon(data = dat2, aes(y = prediction, ymin = lwr,
                                 ymax = upr,
                                 fill = warming),
                alpha = 0.1,
                linetype = 0) +
    geom_point(size = 2) +
    # prediction line
    geom_line(data = dat2, aes(y = prediction), linewidth = 0.5) +
    labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
         y = yaxislabel) +
    # scales
    scale_colour_manual(name = "Warming", values = colourpalette) +
    scale_fill_manual(name = "Warming", values = colourpalette) +
    scale_linetype_manual(name = "Clipping", values = linetypepalette) +
    scale_shape_manual(name = "Clipping", values = shapepalette) +
    # change labels to real values
    scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
    # facet
    facet_grid(origSiteID ~ .data[[facet_2]], scales = "free") +
    theme_bw() +
    theme(legend.position = "top",
          legend.box ="vertical",
          text = element_text(size = 12))

  if(is.na(facet_2)){
    plot + facet_grid(origSiteID ~ "", scales = "free")
  } else {
    plot
  }

}


# annual climate
make_annual_climate_figure <- function(annual_climate){

  dat <- annual_climate |>
    pivot_wider(names_from = "variable", values_from = c(value, se)) |>
    mutate(siteID = factor(siteID, levels = c("Vikesland", "Joasete", "Liahovden")))

  means <- dat |>
    group_by(siteID) |>
    summarise(prec = mean(value_precipitation),
              temp = mean(value_temperature))

  annual_climate_figure <- dat |>
    ggplot(aes(x = value_precipitation, y = value_temperature, shape = siteID, colour = year)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = value_temperature - se_temperature,
                      ymax = value_temperature + se_temperature)) +
    geom_errorbarh(aes(xmin = value_precipitation - se_precipitation,
                       xmax = value_precipitation + se_precipitation)) +
    geom_point(data = means, aes(x = prec, y = temp), colour = "#E69F00", size = 5) +
    scale_colour_viridis_c() +
    scale_shape_manual(values = c(15, 16, 17)) +
    labs(x = "Annual precipitation in mm", y = "Summer temperature in °C") +
    theme_bw() +
    theme(legend.position = "top",
          legend.box ="vertical",
          text = element_text(size = 17))

  return(annual_climate_figure)

}


# daily climate in control and warming
make_daily_climate_figure <- function(daily_temp, col_palette){

  # Daily climate: split into temperature and soil moisture
  daily_temp_plot <- daily_temp |>
    filter(Nlevel == 1,
           grazing == "Control",
           variable %in% c("air", "ground", "soil")) |>
    ggplot(aes(x = date, y = value, colour = warming)) +
    geom_line() +
    scale_color_manual(name = "", values = col_palette) +
    labs(x = "", y = "Temperature (°C)", tag = "a)") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "top")

  daily_moisture_plot <- daily_temp |>
    filter(Nlevel == 1,
           grazing == "Control",
           variable == "soilmoisture") |>
    ggplot(aes(x = date, y = value, colour = warming)) +
    geom_line() +
    scale_color_manual(name = "", values = col_palette) +
    labs(x = "", y = "Soil moisture (%)") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none")

  # Summer mean data
  summer_mean <- daily_temp |>
    mutate(month = month(date),
           year = year(date)) |>
    filter(month %in% c(5, 6, 7, 8, 9),
           grazing == "Control",
           Namount_kg_ha_y == 0)

  # stats
  summer_mean |>
    group_by(variable, origSiteID) |>
    nest() |>
    mutate(fit = map(data, ~lm(value ~ warming, data = .)),
           res = map(fit, tidy)) |>
    unnest(res)

  # Summer mean climate: split into temperature and soil moisture
  summer_temp_plot <- summer_mean |>
    filter(variable %in% c("air", "ground", "soil")) |>
    ggplot(aes(x = warming, y = value, fill = warming)) +
    geom_violin(draw_quantiles = c(0.5)) +
    scale_fill_manual(name = "", values = col_palette) +
    annotate("text", x = Inf, y = Inf, label = "*", hjust = 1, vjust = 1, size = 10, colour = col_palette[2]) +
    labs(x = "", y = "Temperature (°C)", tag = "b)") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")

  summer_moisture_plot <- summer_mean |>
    filter(variable == "soilmoisture") |>
    ggplot(aes(x = warming, y = value, fill = warming)) +
    geom_violin(draw_quantiles = c(0.5)) +
    scale_fill_manual(name = "", values = col_palette) +
    annotate("text", x = Inf, y = Inf, label = "*", hjust = 1, vjust = 1, size = 10, colour = col_palette[2]) +
    labs(x = "", y = "Soil moisture (%)") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")

  # Combine all with patchwork: columns = daily (left) and summer (right)
  daily_group <- (daily_temp_plot / daily_moisture_plot) +
    plot_layout(heights = c(3, 1))

  summer_group <- (summer_temp_plot / summer_moisture_plot) +
    plot_layout(heights = c(3, 1))

  daily_climate_figure <- (daily_group | summer_group) +
    plot_layout(widths = c(3, 1.5), guides = "collect") &
    theme(legend.position = "bottom")

  return(daily_climate_figure)

}



# climate treatment figure
make_climate_treatment_figure <- function(daily_temp){

  grazing <- daily_temp |>
    mutate(month = month(date),
           year = year(date)) |>
    filter(month %in% c(5, 6, 7, 8, 9),
           warming == "Ambient",
           Namount_kg_ha_y == 0) |>
    ggplot(aes(x = grazing, y = value, fill = grazing)) +
    geom_violin(draw_quantiles = 0.5) +
    scale_fill_manual(values = c("grey", "coral", "coral4")) +
    labs(x = "",
         tag = "a)") +
    facet_grid2(origSiteID ~ variable, scales = "free_y", independent = "y") +
    theme_bw() +
    theme(legend.position = "none")


  nitrogen <- daily_temp |>
    mutate(month = month(date),
           year = year(date)) |>
    filter(month %in% c(5, 6, 7, 8, 9),
           warming == "Ambient",
           grazing == "Control") |>
    ggplot(aes(x = factor(Namount_kg_ha_y), y = value, fill = factor(Namount_kg_ha_y))) +
    geom_violin(draw_quantiles = 0.5) +
    scale_fill_viridis_d(option = "inferno", direction = -1) +
    labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
         tag = "b)") +
    facet_grid2(origSiteID ~ variable, scales = "free_y", independent = "y") +
    theme_bw() +
    theme(legend.position = "none")

  climate_treatment <- grazing / nitrogen

  return(climate_treatment)
}




# daily_temp |>
#   mutate(month = month(date),
#          year = year(date)) |>
#   filter(month %in% c(5, 6, 7, 8, 9),
#          grazing == "C",
#          Namount_kg_ha_y == 0) |>
#   group_by(variable) |>
#   nest(data = -variable) %>%
#   mutate(fit = map(data, ~ lm(value ~ warming, data = .x)),
#          fit_tidy = map(fit, tidy)) |>
#   unnest(fit_tidy)
#
# dd <- daily_temp |>
#   mutate(month = month(date),
#          year = year(date)) |>
#   filter(month %in% c(5, 6, 7, 8, 9),
#          grazing == "C",
#          Namount_kg_ha_y == 0,
#          variable == "soilmoisture")
# fit <- lm(value ~ warming * origSiteID, data = dd)
# check_model(fit)
#
#
# daily_temp |>
#   mutate(month = month(date),
#          year = year(date)) |>
#   filter(month %in% c(5, 6, 7, 8, 9),
#          warming == "Ambient",
#          Namount_kg_ha_y == 0) |>
#   group_by(variable) |>
#   nest(data = -variable) %>%
#   mutate(fit = map(data, ~ lm(value ~ grazing, data = .x)),
#          fit_tidy = map(fit, tidy)) |>
#   unnest(fit_tidy)
#
# daily_temp |>
#   mutate(month = month(date),
#          year = year(date)) |>
#   filter(month %in% c(5, 6, 7, 8, 9),
#          warming == "Ambient",
#          grazing == "C") |>
#   group_by(variable) |>
#   nest(data = -variable) %>%
#   mutate(fit = map(data, ~ lm(value ~ Namount_kg_ha_y, data = .x)),
#          fit_tidy = map(fit, tidy)) |>
#   unnest(fit_tidy)
