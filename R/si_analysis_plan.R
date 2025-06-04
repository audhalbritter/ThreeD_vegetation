# make analysis
si_analysis_plan <- list(

  ## ESTIMATE STANDING BIOMASS
  tar_target(
    name = standing_biomass_data,
    command = estimated_standing_biomass |>
        select(-sum_cover, -height) |>
        filter(year == 2022) |>
        # join collected biomass from control plots
        tidylog::left_join(measured_standing_biomass |>
                             filter(grazing == "Control"))
  ),

  tar_target(
    name = standing_biomass_model,
    command = {
      # Linear model
      fit <- lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log, data = standing_biomass_data |>
                  filter(grazing == "Control",
                         year == 2022))
    }
  ),

  tar_target(
    name = standing_biomass_model_output,
    command = summary(SB_back_model_22)
  ),


  # MICROCLIMATE
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
                     names_to = c(".value", "effects", "names")) |>
        unnest(glance) |>
        select(variable:adj.r.squared, AIC) |>
        # select only interaction models
        filter(effects == "interaction") |>
        # select best model (BY HAND!!!)
        filter(names == "log")
      #filter(AIC == min(AIC)) # normally one would do this

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
  )

  # tar_target(
  #   name = microclimate_save,
  #   command = microclimate_stats |>
  #     gtsave("output/microclimate_stats.png", expand = 10)
  # )

)