### Summer site climate summaries (shared by methods + SI Table S2)

# Plot subset: May–September, unfertilized control plots, full logger period.
# Alpine warming transplants are excluded so each destination site reflects
# ambient (or destination) climate used in Table S2.
summarise_summer_site_climate <- function(daily_temp) {
  daily_temp |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::filter(
      month %in% 5:9,
      Nlevel %in% c(1, 2, 3),
      grazing == "Control",
      !(origSiteID == "Alpine" & warming == "Warming")
    ) |>
    dplyr::mutate(
      siteID = dplyr::case_when(
        destSiteID == "Liahovden" ~ "Alpine",
        destSiteID == "Joasete" ~ "Sub-alpine",
        destSiteID == "Vikesland" ~ "Boreal"
      ),
      siteID = factor(siteID, levels = c("Alpine", "Sub-alpine", "Boreal"))
    ) |>
    dplyr::group_by(variable, siteID) |>
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      se = stats::sd(value, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )
}

# Mean temperature step between adjacent sites (± SE of that mean step).
# Expects output of summarise_summer_site_climate().
summarise_summer_site_temp_diff <- function(site_climate,
                                           digits_est = 2,
                                           digits_se = 3) {
  site_climate |>
    dplyr::filter(variable %in% c("air", "ground", "soil")) |>
    tidyr::pivot_wider(names_from = siteID, values_from = c(mean, se)) |>
    dplyr::mutate(
      estimate = ((`mean_Sub-alpine` - mean_Alpine) + (mean_Boreal - `mean_Sub-alpine`)) / 2,
      std.error = sqrt(
        (`se_Sub-alpine`^2 + se_Alpine^2) + (se_Boreal^2 + `se_Sub-alpine`^2)
      ) / 2,
      label = paste0(round(estimate, digits_est), " ± ", round(std.error, digits_se))
    ) |>
    dplyr::select(variable, estimate, std.error, label)
}
