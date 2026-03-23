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

  # compare biomass models
  tar_target(
    name = biomass_estimation_model_comparison,
    command = {
      # Prepare data
      data <- prep_SB_back |>
        filter(
          grazing == "Control",
          year == 2022
        )
      
      # Fit all models
      model_full <- lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log * warming, data = data)
      model_additive <- lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log + warming, data = data)
      model_N <- lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log, data = data)
      model_W <- lm(biomass_remaining_coll ~ biomass_remaining_calc + warming, data = data)
      model_biomass <- lm(biomass_remaining_coll ~ biomass_remaining_calc, data = data)
      
      # Compare by AIC
      aic_comparison <- data.frame(
        Model = c("biomass + N * W", "biomass + N + W", "biomass + N", "biomass + W", "biomass"),
        AIC = c(
          AIC(model_full),
          AIC(model_additive),
          AIC(model_N),
          AIC(model_W),
          AIC(model_biomass)
        ),
        R2 = c(
          summary(model_full)$r.squared,
          summary(model_additive)$r.squared,
          summary(model_N)$r.squared,
          summary(model_W)$r.squared,
          summary(model_biomass)$r.squared
        )
      ) |>
        arrange(AIC) |>
        mutate(delta_AIC = AIC - min(AIC))
      
      # Return list with models and comparison
      list(
        models = list(
          full = model_full,
          additive = model_additive,
          N = model_N,
          W = model_W,
          biomass = model_biomass
        ),
        aic_comparison = aic_comparison
      )
    }
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