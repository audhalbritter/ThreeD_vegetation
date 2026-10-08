# make analysis
si_analysis_plan <- list(

  ## ESTIMATE STANDING BIOMASS
  # Subplots with harvested biomass used to calibrate the 2022 back-transform (control, 2022).
  tar_target(
    name = n_calibration_biomass_plots,
    command = prep_SB_back |>
      filter(year == 2022, grazing == "Control", !is.na(biomass_remaining_coll)) |>
      nrow()
  ),

  # Summary of SB_back_model_22 (same fit as transformation_plan.R).
  tar_target(
    name = standing_biomass_model_output,
    command = summary(SB_back_model_22)
  ),

  # Mean Carex cover (% of summed vascular cover) across subplots at final survey (2022).
  tar_target(
    name = mean_carex_cover_pct,
    command = cover_total |>
      filter(year == 2022) |>
      group_by(turfID) |>
      summarise(
        carex_cover = sum(cover[stringr::str_detect(species, "^Carex")], na.rm = TRUE),
        total_cover = sum(cover, na.rm = TRUE),
        carex_pct = if_else(
          total_cover > 0,
          100 * carex_cover / total_cover,
          NA_real_
        ),
        .groups = "drop"
      ) |>
      summarise(mean_carex_pct = mean(carex_pct, na.rm = TRUE)) |>
      pull(mean_carex_pct)
  ),

  # MICROCLIMATE
  # Summer (May–September) site climate for Table S2 / methods site differences
  tar_target(
    name = summer_site_climate,
    command = summarise_summer_site_climate(daily_temp)
  ),

  tar_target(
    name = summer_site_temp_diff,
    command = summarise_summer_site_temp_diff(summer_site_climate)
  ),

  # run 3-way interaction model for climate
  tar_target(
    name = climate_model,
    command = {

      daily_temp2 <- as.data.frame(daily_temp)

      average_summer_climate <- daily_temp2 |>
        mutate(month = month(date),
               year = year(date)) |>
        filter(month %in% c(5, 6, 7, 8, 9)) |>
        group_by(variable, origSiteID, warming, grazing, Namount_kg_ha_y) |>
        summarise(value = mean(value)) |>
        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
               grazing_num = as.numeric(grazing_num)) |>
        # log transform Nitrogen
        mutate(Nitrogen_log = log(Namount_kg_ha_y + 1))

      run_full_model(dat = average_summer_climate |>
                       filter(grazing != "Natural"),
                     group = c("origSiteID", "variable"),
                     response = value,
                     grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, variable, data),
                     names_sep = "_",
                     names_to = c(".value", "names")) |>
        unnest(glance) |>
        select(variable:adj.r.squared, AIC) |>
        # select log model, because usually the best fit
        filter(names == "log")

    }

  ),

  # prediction and model output
  tar_target(
    name = climate_output,
    command = make_prediction(climate_model)

  ),

  # prepare model output
  tar_target(
    name = climate_prediction,
    command = climate_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, variable, output) |>
      unnest(output) |>
      rename(prediction = fit) #|>
    # mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))
  ),


  # stats
  tar_target(
    name =   climate_anova_table,
    command = climate_output |>
      select(origSiteID, variable, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = climate_summary_table,
    command = climate_output |>
      select(origSiteID, variable, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = climate_stats,
    command = make_climate_stats(climate_anova_table)
  ),

  tar_target(
    name = microclimate_stats,
    command = make_microclimate_stats(as.data.frame(daily_temp))
  ),

  # tar_target(
  #   name = microclimate_save,
  #   command = microclimate_stats |>
  #     gtsave("output/microclimate_stats.png", expand = 10)
  # )

  # Species gained or lost under warming, compared against the full site-level
  # species pool of all ambient or all warming plots
  # (ungrazed, unfertilized plots only, year 2022)
  tar_target(
    name = species_turnover_warming,
    command = {
      presence <- cover_total |>
        filter(
          year == 2022,
          grazing == "Control",
          Namount_kg_ha_y == 0
        ) |>
        distinct(origSiteID, warming, species)

      # Full species pool across all ambient plots per site
      ambient_pool <- presence |>
        filter(warming == "Ambient") |>
        select(origSiteID, species)

      # Full species pool across all warming plots per site
      warming_pool <- presence |>
        filter(warming == "Warming") |>
        select(origSiteID, species)

      # Species lost: in ambient pool but absent from all warming plots at that site
      lost <- ambient_pool |>
        anti_join(warming_pool, by = c("origSiteID", "species")) |>
        mutate(status = "lost")

      # Species gained: in warming pool but absent from all ambient plots at that site
      gained <- warming_pool |>
        anti_join(ambient_pool, by = c("origSiteID", "species")) |>
        mutate(status = "gained")

      bind_rows(lost, gained) |>
        arrange(origSiteID, status, species)
    }
  )

)