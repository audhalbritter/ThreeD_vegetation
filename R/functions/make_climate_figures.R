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

  daily_climate_figure <- daily_temp |>
    filter(Nlevel == 1,
           grazing == "C") |>
    mutate(origSiteID = recode(origSiteID, Joa = "Sub-alpine", Lia = "Alpine"),
           variable = recode(variable, air_temperature = "air", ground_temperature = "ground", soil_temperature = "soil")) |>
    ggplot(aes(x = date, y = value, colour = warming)) +
    geom_line() +
    scale_color_manual(values = c("grey", "red")) +
    labs(x = "", y = "soilmoisture in % or temperature in °C") +
    facet_grid(variable ~ origSiteID, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "top")

  return(daily_climate_figure)
}


