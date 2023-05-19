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
    name = mean_summer_climate,
    command = daily_temp |>
      mutate(month = month(date),
             year = year(date)) %>%
      filter(month %in% c(6, 7, 8, 9),
             year %in% c(2020, 2021),
             # only controls
             Nlevel %in% c(1, 2, 3),
             grazing == "Control") %>%
      group_by(year, variable, origSiteID, warming) %>%
      summarise(mean = mean(value),
                se = sd(value)/sqrt(n())) |>
      pivot_wider(names_from = warming, values_from = c(mean, se)) |>
      mutate(diff = mean_Warming - mean_Ambient) |>
      mutate(destSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) |>
      arrange(year, variable, destSiteID)
  ),

  ### PRODUCTIVITY

  tar_target(
    name = productivity_model,
    command = {

      # sum fun groups
      total_productivity <- productivity |>
        # remove litter, because it is not really productivity
        filter(!fun_group %in% c("litter")) |>
        ungroup() |>
        group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year) |>
        summarise(sum_productivity = sum(productivity))

      # test productiviy in 2022
      productivity_model <- run_full_model(dat = total_productivity |>
                                              filter(grazing != "Natural",
                                                     year == 2022),
                                            group = c("origSiteID"),
                                            response = sum_productivity,
                                            grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, data),
                     names_sep = "_",
                     names_to = c(".value", "names")) |>
        unnest(glance) |>
        select(origSiteID:adj.r.squared, AIC) |>
        # select best model
        filter(AIC == min(AIC))
    }
  ),

  # check models
  tar_quarto(name = model_check,
             path = "R/model_output.qmd"),


  tar_target(
    name = productivity_output,
    command = make_prediction(productivity_model)

  ),

  # prepare model output
  tar_target(
    name =   productivity_prediction,
    command = productivity_output |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   productivity_stats,
    command = productivity_output |>
      select(origSiteID, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  ### FUNCTIONAL GROUP COVER
  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = cover_model,
    command = run_full_model(dat = functional_group_cover |>
                               filter(#!functional_group %in% c("legume", "shrub"),
                                      grazing != "Natural"),
                             group = c("origSiteID", "functional_group"),
                             response = delta,
                             grazing_var = grazing_num) |>
      # make long table
      pivot_longer(cols = -c(origSiteID, functional_group, data),
                   names_sep = "_",
                   names_to = c(".value", "names")) |>
      unnest(glance) |>
      select(origSiteID:adj.r.squared, AIC) |>
      # select best model
      filter(AIC == min(AIC))
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
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, functional_group, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))
  ),

  # stats
  tar_target(
    name =   cover_stats,
    command = cover_output |>
      select(origSiteID, functional_group, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),


  # # bryophytes, lichen and litter
  # tar_target(
  #   name = cover_bryo_model,
  #   command = run_full_model(dat = comm_structure |>
  #                              filter(functional_group %in% c("Litter", "Bryophytes", "Lichen"),
  #                                     grazing != "Natural"),
  #                            group = c("origSiteID", "functional_group"),
  #                            response = delta,
  #                            grazing_var = grazing_num) |>
  #     # make long table
  #     pivot_longer(cols = -c(origSiteID, functional_group, data),
  #                  names_sep = "_",
  #                  names_to = c(".value", "names")) |>
  #     # select best model
  #     filter(aic == min(aic))
  # ),
  #
  # tar_target(
  #   name = cover_bryo_output,
  #   command = make_prediction(cover_bryo_model)
  # ),
  #
  # # prepare model output
  # tar_target(
  #   name = cover_bryo_prediction,
  #   command = cover_bryo_output |>
  #     # merge data and prediction
  #     mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
  #     select(origSiteID, functional_group, output) |>
  #     unnest(output) |>
  #     rename(prediction = fit) |>
  #     mutate(functional_group = factor(functional_group, levels = c("Bryophytes", "Lichen", "Litter")))
  # ),
  #
  #
  # # stats
  # tar_target(
  #   name =   cover_bryo_stats,
  #   command = cover_bryo_output |>
  #     unnest(result) |>
  #     ungroup() |>
  #     select(-data, -model, -aic, -prediction) |>
  #     fancy_stats()
  # ),


  ### DIVERSITY
  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = diversity_model,
    command = {

      models <- run_full_model(dat = diversity |>
                       filter(grazing != "Natural"),
                     group = c("origSiteID", "diversity_index"),
                     response = delta,
                     grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "names")) |>
        unnest(glance) |>
        select(origSiteID:adj.r.squared, AIC) |>

      # models |>
        # select best model
        filter(AIC == min(AIC)) #|>
        # filter(!(origSiteID == "Alpine" & diversity_index == "richness")) |>
        # # force quadratic model for alpine richness, because it visually fits better & R squared is higher
        # bind_rows(models |>
        #             filter(origSiteID == "Alpine" & diversity_index == "richness" & names == "quadratic"))

    }


  ),

  tar_target(
    name = diversity_output,
    command = make_prediction(diversity_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_prediction,
    command = diversity_output |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),


  # stats
  tar_target(
    name =   diversity_stats,
    command = diversity_output |>
      select(origSiteID, diversity_index, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  )


  # # model selection cover and grazing levels
  # tar_target(
  #   name = cover_model_selection,
  #   command = do_model_selection(functional_group_cover)
  #   ),
  #
  # tar_target(
  #   name = cover_result,
  #   command = {
  #
  #     dat <- functional_group_cover |>
  #       filter(grazing != "Natural",
  #              functional_group %in% c("graminoid", "forb")) |>
  #
  #       # best model
  #       mutate(best_model = case_when(origSiteID == "Alpine" & functional_group == "graminoid" ~ "delta ~ grazing_num * warming + Nitrogen_log * warming",
  #                                     origSiteID == "Alpine" & functional_group == "forb" ~ "delta ~ Nitrogen_log * warming",
  #                                     origSiteID == "Sub-alpine" & functional_group == "graminoid" ~ "delta ~ grazing_num + Nitrogen_log + warming",
  #                                     origSiteID == "Sub-alpine" & functional_group == "forb" ~ "delta ~ Nitrogen_log + warming")) |>
  #       group_by(origSiteID, functional_group, best_model)
  #
  #       cover_result <- run_best_models(dat)
  #
  #   }
  # ),

)
