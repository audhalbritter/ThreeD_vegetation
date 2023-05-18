# make analysis
si_analysis_plan <- list(

  # HEIGHT
  # tar_target(
  #   name = result_height,
  #   command = {
  #
  #     run_full_model(dat = height |>
  #                      filter(grazing != "Natural"),
  #                    group = c("origSiteID", "vegetation_layer"),
  #                    response = delta,
  #                    grazing_var = grazing_num)
  #   }
  # ),
  #
  # # prepare model output
  # tar_target(
  #   name =   height_model_output,
  #   command = result_height |>
  #     # merge data and prediction
  #     mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
  #     select(origSiteID, output) |>
  #     unnest(output) |>
  #     rename(prediction = fit)
  # ),
  #
  #
  # # stats
  # tar_target(
  #   name =   height_stats,
  #   command = result_height |>
  #     unnest(result) |>
  #     ungroup() |>
  #     select(-data, -model, -prediction) |>
  #     fancy_stats()
  # ),


  ## FUNCTIONAL GROUP COVER
  # natural grazing
  # run 3-way interaction model for cover
  tar_target(
    name = cover_CN_model,
    command = run_full_model(dat = functional_group_cover |>
                               filter(!functional_group %in% c("legume", "shrub"),
                                      grazing %in% c("Control", "Natural")) |>
                               select(-grazing_num),
                             group = c("origSiteID", "functional_group"),
                             response = delta,
                             grazing_var = grazing) |>
      # make long table
      pivot_longer(cols = -c(origSiteID, functional_group, data),
                   names_sep = "_",
                   names_to = c(".value", "names")) |>
      # select best model
      filter(aic == min(aic))
  ),

  # prediction and model output
  tar_target(
    name = cover_CN_output,
    command = make_prediction(cover_CN_model)

  ),

  # prepare model output
  tar_target(
    name = cover_CN_prediction,
    command = cover_CN_output |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, functional_group, output) |>
      unnest(output) |>
      rename(prediction = fit,
             grazing = .grazing) |>
      mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge")))
  ),


  # stats
  tar_target(
    name =   cover_CN_stats,
    command = cover_CN_output |>
      unnest(result) |>
      ungroup() |>
      select(-data, -model, -prediction) |>
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
                   names_to = c(".value", "names")) |>
      # select best model
      filter(aic == min(aic))
  ),

  # prediction and model output
  tar_target(
    name = diversity_CN_output,
    command = make_prediction(diversity_CN_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_CN_prediction,
    command = diversity_CN_output |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit,
             grazing = .grazing) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),

  # stats
  tar_target(
    name =   diversity_CN_stats,
    command = diversity_CN_output |>
      unnest(result) |>
      ungroup() |>
      select(-data, -model, -prediction)  |>
      fancy_stats()
  )

)
