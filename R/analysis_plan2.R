# make analysis for Nitrogen_log
analysis_plan2 <- list(

  ### PRODUCTIVITY

  tar_target(
    name = productivity_model2,
    command = {

      # sum fun groups
      total_productivity <- productivity |>
        # remove litter, because it is not really productivity
        filter(!fun_group %in% c("litter")) |>
        ungroup() |>
        group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year) |>
        summarise(sum_productivity = sum(productivity))

      # test productivity in 2022
      productivity_model <- run_model(dat = total_productivity |>
                                              filter(grazing != "Natural",
                                                     year == 2022),
                                            group = c("origSiteID"),
                                            response = sum_productivity,
                                            grazing_var = grazing_num)
    }
  ),


  tar_target(
    name = productivity_output2,
    command = make_prediction(productivity_model2)

  ),

  # prepare model output
  tar_target(
    name =   productivity_prediction2,
    command = productivity_output2 |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   productivity_stats2,
    command = productivity_output2 |>
      unnest(result) |>
      ungroup() |>
      select(-data, -model, -prediction) %>%
      fancy_stats(.)
  ),

  ### FUNCTIONAL GROUP COVER
  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = cover_model2,
    command = run_model(dat = functional_group_cover |>
                               filter(!functional_group %in% c("legume", "shrub"),
                                      grazing != "Natural"),
                             group = c("origSiteID", "functional_group"),
                             response = delta,
                             grazing_var = grazing_num)
  ),

  # prediction and model output
  tar_target(
    name = cover_output2,
    command = make_prediction(cover_model2)

  ),

  # prepare model output
  tar_target(
    name = cover_prediction2,
    command = cover_output2 |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, functional_group, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb")))
  ),

  # stats
  tar_target(
    name =   cover_stats2,
    command = cover_output2 |>
      unnest(result) |>
      ungroup() |>
      select(-data, -model, -prediction) |>
      fancy_stats()
  ),

  ### DIVERSITY
  # grazing intensity
  # run 3-way interaction model for cover
  tar_target(
    name = diversity_model2,
    command = models <- run_model(dat = diversity |>
                       filter(grazing != "Natural"),
                     group = c("origSiteID", "diversity_index"),
                     response = delta,
                     grazing_var = grazing_num)

  ),

  tar_target(
    name = diversity_output2,
    command = make_prediction(diversity_model2)

  ),

  # prepare model output
  tar_target(
    name = diversity_prediction2,
    command = diversity_output2 |>
      # merge data and prediction
      mutate(output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),


  # stats
  tar_target(
    name =   diversity_stats2,
    command = diversity_output2 |>
      unnest(result) |>
      ungroup() |>
      select(-data, -model, -prediction) |>
      fancy_stats()
  )

)
