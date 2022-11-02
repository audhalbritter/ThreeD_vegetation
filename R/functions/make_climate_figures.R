### make climate figures

# annual climate
make_annual_climate_figure <- function(annual_climate){

  annual_climate_figure <- annual_climate |>
    pivot_wider(names_from = "variable", values_from = c(value, se)) |>
    mutate(siteID = factor(siteID, levels = c("Vikesland", "Joasete", "Liahovden"))) |>
    ggplot(aes(x = value_precipitation, y = value_temperature, shape = siteID, colour = year)) +
    geom_point(size = 3) +
    scale_colour_viridis_c() +
    scale_shape_manual(values = c(15, 16, 17)) +
    labs(x = "Annual precipitation in mm", y = "Summer temperature in °C") +
    theme_minimal()

  return(annual_climate_figure)

}


# daily climate in control and warming
make_daily_climate_figure <- function(daily_temp){

  daily_climate <- daily_temp |>
    filter(Nlevel == 1,
           grazing == "C") |>
    mutate(origSiteID = recode(origSiteID, Joa = "Sub-alpine", Lia = "Alpine"),
           variable = recode(variable, air_temperature = "air", ground_temperature = "ground", soil_temperature = "soil")) |>
    ggplot(aes(x = date, y = value, colour = warming)) +
    geom_line() +
    scale_color_manual(values = c("grey", "#CC79A7")) +
    labs(x = "", y = "soilmoisture in %                           temperature in °C") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "top")

  summer_mean <- daily_temp |>
    mutate(month = month(date),
           year = year(date)) |>
    filter(month %in% c(5, 6, 7, 8, 9),
           grazing == "C",
           Namount_kg_ha_y == 0) |>
    group_by(variable, warming, origSiteID) |>
    summarise(se = sd(value)/sqrt(n()),
              value = mean(value)) |>
    mutate(origSiteID = recode(origSiteID, Joa = "Sub-alpine", Lia = "Alpine"),
           variable = recode(variable, air_temperature = "air", ground_temperature = "ground", soil_temperature = "soil")) |>
    ggplot(aes(x = warming, y = value, colour = warming)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2) +
    scale_colour_manual(values = c("grey", "#CC79A7")) +
    labs(x = "", y = "") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")

  daily_climate_figure <- daily_climate + summer_mean + plot_layout(widths = c(3, 1))

  return(daily_climate_figure)
}



# climate treatment figure
# make_climate_treatment_figure <- function(daily_temp){
#
#   df <- daily_temp |>
#     mutate(month = month(date),
#            year = year(date)) |>
#     filter(month %in% c(5, 6, 7, 8, 9),
#            grazing == "C",
#            Namount_kg_ha_y == 0)
#     warm <- ggplot(df, aes(x = warming, y = value, fill = warming)) +
#     geom_violin(draw_quantiles = 0.5) +
#     annotate("text", x = Inf, y =Inf, label = "*", hjust = 1, vjust = 1, size = 10, colour = "#CC79A7") +
#     scale_fill_manual(values = c("grey", "#CC79A7")) +
#     labs(x = "") +
#     facet_wrap(~ variable, scales = "free_y", nrow = 1) +
#     theme_bw() +
#     theme(legend.position = "none")
#
#   grazing <- daily_temp |>
#     mutate(month = month(date),
#            year = year(date)) |>
#     filter(month %in% c(5, 6, 7, 8, 9),
#            warming == "Ambient",
#            Namount_kg_ha_y == 0) |>
#     mutate(grazing = recode(grazing, C = "Control", M = "Medium", I = "Intensive"),
#            grazing = factor(grazing, levels = c("Control", "Medium", "Intensive"))) |>
#     ggplot(aes(x = grazing, y = value, fill = grazing)) +
#     geom_violin(draw_quantiles = 0.5) +
#     scale_fill_manual(values = c("grey", "coral", "coral4")) +
#     labs(x = "") +
#     facet_wrap(~ variable, scales = "free_y", nrow = 1) +
#     theme_bw() +
#     theme(legend.position = "none")
#
#
#   nitrogen <- daily_temp |>
#     mutate(month = month(date),
#            year = year(date)) |>
#     filter(month %in% c(5, 6, 7, 8, 9),
#            warming == "Ambient",
#            grazing == "C") |>
#     ggplot(aes(x = factor(Namount_kg_ha_y), y = value, fill = factor(Namount_kg_ha_y))) +
#     geom_violin(draw_quantiles = 0.5) +
#     scale_fill_viridis_d(option = "inferno", direction = -1) +
#     labs(x = "") +
#     facet_wrap(~ variable, scales = "free_y", nrow = 1) +
#     theme_bw() +
#     theme(legend.position = "none")
#
#   climate_treatment <- warm / grazing / nitrogen
#
#   return(climate_treatment)
# }




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
