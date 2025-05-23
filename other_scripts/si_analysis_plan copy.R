# make analysis
si_analysis_plan <- list(

  ## MICROCLIMATE
  # run 3-way interaction model for climate
  tar_target(
    name = climate_model,
    command = {

      average_summer_climate <- daily_temp |>
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
    name = microclimate_stats,
    command = make_microclimate_stats(daily_temp)
  ),

  tar_target(
    name = microclimate_save,
    command = microclimate_stats |>
      gtsave("output/microclimate_stats.png", expand = 10)
  ),



  ### PRODUCTIVITY
  tar_target(
    name = productivity_model_all,
    command = {

      # sum fun groups
      total_productivity <- productivity |>
        ungroup() |>
        group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year) |>
        summarise(sum_productivity = sum(productivity))

      # test biomass in 2022
      productivity_model_all <- run_full_model(dat = total_productivity |>
                                                 filter(grazing != "Natural"),
                                               group = c("origSiteID"),
                                               response = sum_productivity,
                                               grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, data),
                     names_sep = "_",
                     names_to = c(".value", "effects", "names")) |>
        unnest(glance) |>
        select(origSiteID:adj.r.squared, AIC, deviance)
    }
  ),

  # only interaction model
  tar_target(
    name = productivity_model,
    command = productivity_model_all |>
      # remove model with single effects
      filter(effects == "interaction") |>
      # select parsimonious model
      filter(AIC == min(AIC))
  ),

  tar_target(
    name = productivity_output,
    command = make_prediction(productivity_model)

  ),

  # prepare model output
  tar_target(
    name =   productivity_prediction,
    command = productivity_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   productivity_anova_table,
    command = productivity_output |>
      select(origSiteID, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = productivity_stats,
    command = make_biomass_stats(productivity_anova_table)
  ),

  tar_target(
    name = productivity_save,
    command = productivity_stats |>
      gtsave("output/productivity_stats.png", expand = 10)
  ),

  # stats
  tar_target(
    name =   productivity_summary_table,
    command = productivity_output |>
      select(origSiteID, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  ## FUNCTIONAL GROUP COVER
  # natural grazing
  # run 3-way interaction model for cover
  tar_target(
    name = cover_CN_model,
    command = run_full_model(dat = functional_group_cover |>
                               filter(grazing %in% c("Control", "Natural")) |>
                               select(-grazing_num),
                             group = c("origSiteID", "functional_group", "group"),
                             response = delta,
                             grazing_var = grazing) |>
      # make long table
      pivot_longer(cols = -c(origSiteID, functional_group, group, data),
                   names_sep = "_",
                   names_to = c(".value", "effects", "names")) |>
      unnest(glance) |>
      select(origSiteID:adj.r.squared, AIC) |>
      # select only interaction models
      filter(effects == "interaction") |>
      # select best model
      filter(origSiteID == "Sub-alpine" & functional_group == "forb" & names == "log" |
               origSiteID == "Sub-alpine" & functional_group == "graminoid" & names == "linear" |
               AIC == min(AIC)) |>
      filter(!(origSiteID == "Sub-alpine" & functional_group == "forb" & names == "linear")) |>
      filter(!(origSiteID == "Sub-alpine" & functional_group == "graminoid" & names == "quadratic"))
    ),

  # prediction and model output
  tar_target(
    name = cover_CN_output,
    command = make_CN_prediction(cover_CN_model)

  ),



  # prepare model output
  tar_target(
    name = cover_CN_prediction,
    command = cover_CN_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, functional_group, output) |>
      unnest(output) |>
      rename(prediction = fit,
             grazing = .grazing) |>
      mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))
  ),


  # stats
  tar_target(
    name =   cover_CN_anova_table,
    command = cover_CN_output |>
      select(origSiteID, functional_group, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = cover_CN_stats,
    command = cover_CN_output |>
      select(origSiteID, functional_group, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),


  # DIVERSITY

  # natural grazing
  # run 3-way interaction model for cover
  tar_target(
    name = diversity_CN_model,
    command = run_full_model(dat = diversity |>
                               filter(grazing %in% c("Control", "Natural")) |>
                               select(-grazing_num),
                             group = c("origSiteID", "diversity_index"),
                             response = delta,
                             grazing_var = grazing)|>
      # make long table
      pivot_longer(cols = -c(origSiteID, diversity_index, data),
                   names_sep = "_",
                   names_to = c(".value", "effects", "names")) |>
      unnest(glance) |>
      select(origSiteID:adj.r.squared, AIC) |>
      # select only interaction models
      filter(effects == "interaction") |>
      # select best model
      # choose log for Alpine evenness and richness because dAIC < 2
      filter(origSiteID == "Alpine" & diversity_index %in% c("evenness", "richness") & names == "log" | AIC == min(AIC)) |>
      filter(!(origSiteID == "Alpine" & diversity_index %in% c("evenness", "richness") & names != "log"))
  ),

  # prediction and model output
  tar_target(
    name = diversity_CN_output,
    command = make_CN_prediction(diversity_CN_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_CN_prediction,
    command = diversity_CN_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit,
             grazing = .grazing) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),

  # stats
  tar_target(
    name =   diversity_CN_anova_table,
    command = diversity_CN_output |>
      select(origSiteID, diversity_index, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name =   diversity_CN_stats,
    command = diversity_CN_output |>
      select(origSiteID, diversity_index, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  ### SEM
  # check estimated and calculated biomass relationship
  tar_target(
    name = biomass_calc_coll_model,
    command = {

      dat <- biomass_div |>
        filter(grazing != "Natural")
      # dat |>
      #   filter(biomass_remaining_coll < 0) |> as.data.frame()
      lm(log(biomass_remaining_calc) ~ log(biomass_remaining_coll + 10)*grazing, data = dat)

    }
  )

)

