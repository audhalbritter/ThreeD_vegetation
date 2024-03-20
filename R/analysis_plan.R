# make analysis
analysis_plan <- list(

  ### CLIMATE
  # annual climate
  tar_target(
    name = site_climate,
    command = annual_climate |>
      group_by(siteID, variable) |>
      summarise(se = sd(value)/sqrt(n()),
                mean = mean(value))
  ),

  # summer temp (note that temp recording in 2019 starts in August! Temp is too low.)
  tar_target(
    name = summer_T,
    command = daily_temp |>
      mutate(month = month(date),
             year = year(date)) %>%
      filter(month %in% c(6, 7, 8, 9),
             year %in% c(2020, 2021),
             # only controls
             Nlevel %in% c(1, 2, 3),
             grazing == "Control") %>%
      group_by(variable, warming) %>%
      summarise(mean = mean(value),
                se = sd(value)/sqrt(n())) |>
      pivot_wider(names_from = warming, values_from = c(mean, se)) |>
      mutate(diff = round((mean_Warming - mean_Ambient), 2),
             se_diff = round((sqrt(se_Warming^2+se_Ambient^2)), 3))
  ),

  ### STANDING BIOMASS
  tar_target(
    name = biomass_model_all,
    command = {

      # sum fun groups
      total_biomass <- standing_biomass |>
        ungroup() |>
        group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year) |>
        summarise(sum_biomass = sum(biomass))

      # test biomass in 2022
      biomass_model_all <- run_full_model(dat = total_biomass |>
                                              filter(grazing != "Natural"),
                                            group = c("origSiteID"),
                                            response = sum_biomass,
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
    name = biomass_model,
    command = biomass_model_all |>
      # remove model with single effects
      filter(effects == "interaction") |>
      # select parsimonious model
      filter(AIC == min(AIC))
  ),

  # # check models
  # tar_quarto(name = model_check,
  #            path = "R/model_output.qmd"),


  tar_target(
    name = biomass_output,
    command = make_prediction(biomass_model)

  ),

  # prepare model output
  tar_target(
    name =   biomass_prediction,
    command = biomass_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   biomass_anova_table,
    command = biomass_output |>
      select(origSiteID, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  # stats
  tar_target(
    name =   biomass_summary_table,
    command = biomass_output |>
      select(origSiteID, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),


  ### FUNCTIONAL GROUP COVER
  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = cover_model_all,
    command = run_full_model(dat = functional_group_cover |>
                               filter(#!functional_group %in% c("legume", "shrub"),
                                      grazing != "Natural"),
                             group = c("origSiteID", "functional_group", "group"),
                             response = delta,
                             grazing_var = grazing_num) |>
      # make long table
      pivot_longer(cols = -c(origSiteID, functional_group, group, data),
                   names_sep = "_",
                   names_to = c(".value", "effects", "names")) |>
      unnest(glance) |>
      select(origSiteID:adj.r.squared, AIC, deviance)
  ),


    # only interaction model
    tar_target(
      name = cover_model,
      command = cover_model_all |>
        filter(effects == "interaction") |>
        # select parsimonious model (or log if dAIC < 2)
        filter((origSiteID == "Sub-alpine" & names == "log") |
                 (origSiteID == "Alpine" & functional_group == "forb" & names == "log") |
                 (origSiteID == "Alpine" & functional_group != "forb" & AIC == min(AIC)))

  ),

  # prediction and model output
  tar_target(
    name = cover_output,
    command = make_prediction(cover_model)

  ),

  # prepare model output
  tar_target(
    name = cover_prediction,
    command = cover_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, functional_group, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))
  ),

  # stats
  tar_target(
    name =  cover_anova_table,
    command = cover_output |>
      select(origSiteID, functional_group, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  # stats
  tar_target(
    name =   cover_summary_table,
    command = cover_output |>
      select(origSiteID, functional_group, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  ### DIVERSITY

  # Does diversity in the control plots differ between alpine and sub-alpine communities?
  tar_target(
    name =   diversity_controls,
    command = {
      diversity_control <- cover_total %>%
        group_by(origSiteID, year, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log) %>%
        summarise(richness = n(),
                  diversity = diversity(cover),
                  evenness = diversity/log(richness)) %>%
        pivot_longer(cols = c(richness, diversity, evenness), names_to = "diversity_index", values_to = "value") |>
        filter(warming == "Ambient",
               grazing == "Control",
               Namount_kg_ha_y == 0,
               year == 2022)

      diversity_control |>
        group_by(diversity_index)|>
        nest() |>
        # run model to check difference between sites
        mutate(model = map(data, ~lm(value ~ origSiteID, data = .)),
               result = map(model, tidy),
               anova = map(model, car::Anova),
               anova_tidy = map(anova, tidy)) |>
        unnest(anova_tidy)

    }),

  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = diversity_model_all,
    command = run_full_model(dat = diversity |>
                       filter(grazing != "Natural"),
                     group = c("origSiteID", "diversity_index"),
                     response = delta,
                     grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "effects", "names")) |>
        unnest(glance) |>
        select(origSiteID:adj.r.squared, AIC, deviance)


  ),

  tar_target(
    name = diversity_model,
    command = diversity_model_all |>
      # select only interaction model
      filter(effects == "interaction") |>
      group_by(origSiteID, diversity_index) |>
      # select parsimonious model (done by hand!!!)
      filter(origSiteID == "Sub-alpine" & names == "linear" |
               origSiteID == "Alpine" & diversity_index == "richness" & names == "linear" |
               origSiteID == "Alpine" & AIC == min(AIC)) |>
      filter(!(origSiteID == "Alpine" & diversity_index == "richness" & names == "quadratic"))


      ),

  tar_target(
    name = diversity_output,
    command = make_prediction(diversity_model) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))

  ),

  # prepare model output
  tar_target(
    name = diversity_prediction,
    command = diversity_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),


  # stats
  tar_target(
    name =   diversity_anova_table,
    command = diversity_output |>
      select(origSiteID, diversity_index, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name =   diversity_summary_table,
    command = diversity_output |>
      select(origSiteID, diversity_index, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  # interactions vs single effects
  tar_target(
    name =   single_vs_interaction,
    command = {

      # select the correct models with interactions and single effects and bind them together
      bind_rows(

        # productivity
        productivity = biomass_model_all |>
          select(origSiteID, effects, names, adj.r.squared) |>
          # inner join with correct models (previously selected based on AIC)
          inner_join(biomass_model |>
                       select(origSiteID, names),
                     join_by(origSiteID, names)) |>
          mutate(group = "biomass"),

        # productivity
        cover = cover_model_all |>
          select(origSiteID, functional_group, effects, names, adj.r.squared) |>
          # inner join with correct models (previously selected based on AIC)
          inner_join(cover_model |>
                       select(origSiteID, functional_group, names),
                     join_by(origSiteID, functional_group, names)) |>
          rename(group = functional_group),

        # diversity
        diversity = diversity_model_all |>
          select(origSiteID, diversity_index, effects, names, adj.r.squared) |>
          # inner join with correct models (previously selected based on AIC)
          inner_join(diversity_model |>
                       select(origSiteID, diversity_index, names),
                     join_by(origSiteID, diversity_index, names)) |>
          rename(group = diversity_index),

        .id = "variable"
      )


    }

  )

)
