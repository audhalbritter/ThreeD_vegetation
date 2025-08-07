# analysis plan

analysis_plan <- list(

  ### STANDING BIOMASS AND DIVERSITY GLOBAL CHANGE MODEL
  

  ### biomass separate by origin

  tar_target(
    name = biomass_model_all_origin,
    command = {

      dd <- biomass_div |>
        filter(grazing != "Natural")

      all <- run_full_model(dat = dd,
                            group = c("origSiteID"),
                            response = final_bio,
                            grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, data),
                     names_sep = "_",
                     names_to = c(".value", "mod", "names")) |>
        unnest(glance) |>
        filter(mod == "interaction") |>
        select(origSiteID:adj.r.squared, AIC)
    }
  ),

  # only interaction model
  tar_target(
    name = biomass_origin_model,
    command = biomass_model_all_origin |>
      # select parsimonious model (by HAND!!!)
      filter(names == "log")
  ),

  tar_target(
    name = biomass_origin_output,
    command =  make_prediction(biomass_origin_model)

  ),

  # prepare model output
  tar_target(
    name =   biomass_origin_prediction,
    command = biomass_origin_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   biomass_origin_anova_table,
    command = biomass_origin_output |>
      select(names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  # stats
  tar_target(
    name =   biomass_origin_summary_table,
    command = biomass_origin_output |>
      select(names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),


  ### DIVERSITY
  # grazing intensity
  # run 3-way interaction model for cover

  # analysis by origin
  tar_target(
    name = diversity_model_all_origin,
    command = {
      dd <- biomass_div |>
        pivot_longer(cols = -c(origSiteID:log_ratio_evenness, delta_bio:final_bio),
                     names_to = "diversity_index",
                     values_to = "value") |>
        filter(grazing != "Natural")

      all <- run_full_model(dat = dd,
                            group = c("origSiteID", "diversity_index"),
                            response = value,
                            grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "mod", "names")) |>
        unnest(glance) |>
        filter(mod == "interaction") |>
        select(origSiteID:adj.r.squared, AIC) |>

      # run_full_model_v3_origin(dat = dd,
      #                   group = c("origSiteID", "diversity_index"),
      #                   response = value,
      #                   grazing_var = grazing_num,
      #                   biomass = final_bio) |>
      #   # make long table
      #   pivot_longer(cols = -c(origSiteID, diversity_index, data),
      #                names_sep = "_",
      #                names_to = c(".value", "mod", "names")) |>
      #   unnest(glance) |>
      #   select(diversity_index:adj.r.squared, AIC) |>
        mutate(diversity_index = case_match(diversity_index,
                                            "final_richness" ~ "richness",
                                            "final_diversity" ~ "diversity",
                                            "final_evenness" ~ "evenness"),
               diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
    }

  ),

  tar_target(
    name = diversity_origin_model,
    command = diversity_model_all_origin |>
      group_by(origSiteID,diversity_index) |> 
      #filter(mod == "interaction")
      # select parsimonious model (by HAND!!!)
      filter(names == "linear")
      # Use quadratic model for Sub-alpine diversity, linear for everything else
      # filter(case_when(
      #   origSiteID == "Sub-alpine" & diversity_index == "diversity" ~ names == "quadratic",
      #   TRUE ~ names == "linear"
      # ))
  ),

  tar_target(
    name = diversity_origin_output,
    command = #make_prediction_v2_origin(diversity_origin_model)
              make_prediction(diversity_origin_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_origin_prediction,
    command = diversity_origin_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   diversity_origin_anova_table,
    command = diversity_origin_output |>
      select(origSiteID, diversity_index, names, anova_tidy) |>
      unnest(anova_tidy) |> 
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name =   diversity_origin_summary_table,
    command = diversity_origin_output |>
      select(origSiteID, diversity_index, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  )

)

