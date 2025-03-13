# analysis plan

analysis_plan <- list(

  ### STANDING BIOMASS AND DIVERSITY GLOBAL CHANGE MODEL
  tar_target(
    name = biomass_model_all,
    command = {

      # test biomass in 2022
      bio_model <- run_full_model_v2(dat = biomass_div |>
                                       filter(grazing != "Natural"),
                                     response = final_bio,
                                     grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(data),
                     names_sep = "_",
                     names_to = c(".value", "names")) |>
        unnest(glance) |>
        select(data:adj.r.squared, AIC)
    }
  ),

  # only interaction model
  tar_target(
    name = biomass_model,
    command = biomass_model_all |>
      # select parsimonious model
      filter(AIC == min(AIC))
  ),

  # # check models
  # tar_quarto(name = model_check,
  #            path = "R/model_output.qmd"),


  tar_target(
    name = biomass_output,
    command =  make_prediction_v2(biomass_model)

  ),

  # prepare model output
  tar_target(
    name =   biomass_prediction,
    command = biomass_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(output) |>
      unnest(output) |>
      rename(prediction = fit)
  ),

  # stats
  tar_target(
    name =   biomass_anova_table,
    command = biomass_output |>
      select(names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  # stats
  tar_target(
    name =   biomass_summary_table,
    command = biomass_output |>
      select(names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  ### biomass by origin

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

  # run main effects model first
  tar_target(
    name = diversity_main_model,
    command = {

      run_full_model_v2(dat = biomass_div |>
                       filter(grazing != "Natural"),
                        response = final_diversity,
                        grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(data),
                     names_sep = "_",
                     names_to = c(".value", "names")) |>
        unnest(glance) |>
        select(names:adj.r.squared, AIC)
    }

  ),

  tar_target(
    name = diversity_model_all,
    command = {
      dd <- biomass_div |>
        pivot_longer(cols = -c(origSiteID:log_ratio_evenness, delta_bio:final_bio),
                     names_to = "diversity_index",
                     values_to = "value") |>
          filter(grazing != "Natural")

      run_full_model_v3(dat = dd,
                     group = c("diversity_index"),
                     response = value,
                     grazing_var = grazing_num,
                     biomass = final_bio) |>
        # make long table
        pivot_longer(cols = -c(diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "mod", "names")) |>
        unnest(glance) |>
        select(diversity_index:adj.r.squared, AIC) |>
        mutate(diversity_index = case_match(diversity_index,
                                            "final_richness" ~ "richness",
                                            "final_diversity" ~ "diversity",
                                            "final_evenness" ~ "evenness"),
               diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
    }

  ),

  tar_target(
    name = diversity_interaction_model,
    command = diversity_model_all |>
      group_by(diversity_index) |>
      filter(mod == "interaction")
  ),

  tar_target(
    name = diversity_output,
    command = make_prediction_v2(diversity_interaction_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_prediction,
    command = diversity_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit) #|>
      #mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
  ),


  # stats
  tar_target(
    name =   diversity_anova_table,
    command = diversity_output |>
      select(diversity_index, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name =   diversity_summary_table,
    command = diversity_output |>
      select(diversity_index, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),


  # analysis by origin
  tar_target(
    name = diversity_model_all_origin,
    command = {
      dd <- biomass_div |>
        pivot_longer(cols = -c(origSiteID:log_ratio_evenness, delta_bio:final_bio),
                     names_to = "diversity_index",
                     values_to = "value") |>
        filter(grazing != "Natural")

      run_full_model_v3_origin(dat = dd,
                        group = c("origSiteID", "diversity_index"),
                        response = value,
                        grazing_var = grazing_num,
                        biomass = final_bio) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "mod", "names")) |>
        unnest(glance) |>
        select(diversity_index:adj.r.squared, AIC) |>
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
      group_by(diversity_index) |>
      filter(mod == "interaction")
  ),

  tar_target(
    name = diversity_origin_output,
    command = make_prediction_v2_origin(diversity_origin_model)

  ),

  # prepare model output
  tar_target(
    name = diversity_origin_prediction,
    command = diversity_origin_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, diversity_index, output) |>
      unnest(output) |>
      rename(prediction = fit) #|>
    #mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness")))
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
  ),


  # tar_target(
  #   name = diversity_model,
  #   command = diversity_model_all_origin |>
  #     group_by(diversity_index, origSiteID, mod) |>
  #     arrange(origSiteID, diversity_index, mod, AIC) |>
  #     # get delta AIC and keep all models within delta 2
  #     mutate(delta_AIC = AIC - min(AIC)) |>
  #     filter(delta_AIC <= 2) |>
  #     # Keep linear model if there are 2 models
  #     mutate(n = n()) |>
  #     filter(n == 1 | n == 2 & names == "linear")
  # ),

  ## Figure 2
  tar_target(
    name = fig,
    command = {

      bind_rows(
        Full = diversity_model_all |>
          filter(diversity_index == "diversity") |>
          ungroup(),
        origin = diversity_model_all_origin |>
          filter(diversity_index == "diversity") |>
          ungroup(),
        .id = "model"
      ) |>
        select(model, origSiteID, mod, adj.r.squared) |>
        mutate(model = if_else(model == "origin", origSiteID, model)) |>
        pivot_wider(names_from = mod, values_from = c(adj.r.squared)) |>
        mutate(biomass = bio,
               "main effects" = biomain - bio,
               interactions = biointeraction - bio,
               model = factor(model, levels = c("Full", "Sub-alpine", "Alpine"))) |>
        pivot_longer(cols = c(biomass, "main effects", interactions), names_to = "term", values_to = "value") |>
        ggplot(aes(x = model, y = value, fill = term)) +
        geom_col() +
        scale_fill_manual(name = "", values = c("darkgreen", "orange", "blue")) +
        theme_bw()

    }
  ),

  # Biomass vs. diversity analysis
  tar_target(
    name = standingB_div_final_model,
    command = lm(final_diversity ~ log(final_bio) * origSiteID, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_final_prediction,
    command = augment(standingB_div_final_model)
  ),

  tar_target(
    name = standingB_div_change_model,
    command = lm(log_ratio_diversity ~ log_ratio_bio * origSiteID, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_change_prediction,
    command = augment(standingB_div_change_model)
  ),

  # status: winners, losers, increasing, decreasing and stable species
  # mark status of species in cover data
  tar_target(
    name = cover_wl,
    command = get_winners_and_losers(cover_total)
  ),

  # impute traits for whole community (trait_fill)
  tar_target(
    name = trait_impute_all,
    command = make_trait_impute2(cover_wl |>
                                   filter(year == "2022"),
                                 trait_raw,
                                 ellenberg)
  ),

  # bootstrap
  tar_target(
    name = trait_mean_all,
    command = make_bootstrapping(trait_impute_all)
  ),

  # traits for different stages
  tar_target(
    name = trait_mean_status,
    command = {

      dat <- cover_wl |>
        # remove duplicates
        tidylog::filter(!c(year == 2019 & status %in% c("decrease", "stable", "increase"))) |>
        mutate(status = fct_relevel(status, "loser", "decrease", "stable", "increase", "winner"))

      dat |>
        group_by(status) |>
        nest() |>
        mutate(trait_impute = map(data, ~ make_trait_impute2(.x, trait_raw, ellenberg)),
               trait_mean = map(trait_impute, ~ make_bootstrapping(.x)))


    }
  )

)

