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

  ## STANDING BIOMASS AND GLOBAL CHANGE MODEL

  tar_target(
    name = biomass_model_all,
    command = {

      # test biomass in 2022
      biomass_model_all <- run_full_model(dat = standing_biomass_back |>
                                            filter(grazing != "Natural",
                                                   year == 2022) |>
                                            mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
                                                   grazing_num = as.numeric(grazing_num)),
                                          group = c("origSiteID"),
                                          response = standing_biomass,
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
      # log model is the best or within delta AIC of 2
      filter(names == "log")
      # select parsimonious model
      #filter(AIC == min(AIC))
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


  tar_target(
    name = SEM_origin_final_fig,
    command = {

      # origin cutting
      dat_alp <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Alpine"),
                               landuse = "cutting",
                               diversity = final_diversity,
                               biomass = final_bio,
                               change = FALSE)

      sem_alp <- run_origin_SEM(data = dat_alp,
              landuse = "cutting")

      sem_alp_res <- summary(sem_alp)

      p1 <- make_SEM_figure(sem_results = sem_alp_res,
                      landuse = "cutting")

      dat_sub <- prep_SEM_data(data = biomass_div |>
                             filter(origSiteID == "Sub-alpine"),
                           landuse = "cutting",
                           diversity = final_diversity,
                           biomass = final_bio,
                           change = FALSE)

      sem_sub <- run_origin_SEM(data = dat_sub,
                         landuse = "cutting")

      sem_sub_res <- summary(sem_sub)

      p2 <- make_SEM_figure(sem_results = sem_sub_res,
                      landuse = "cutting")

      # origin grazing
      dat_alp2 <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Alpine"),
                               landuse = "grazing",
                               diversity = final_diversity,
                               biomass = final_bio,
                               change = FALSE)

      sem_alp2 <- run_origin_SEM(data = dat_alp2,
                                 landuse = "grazing")

      sem_alp_res2 <- summary(sem_alp2)

      p3 <- make_SEM_figure(sem_results = sem_alp_res2,
                            landuse = "grazing")

      dat_sub <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Sub-alpine"),
                               landuse = "grazing",
                               diversity = final_diversity,
                               biomass = final_bio,
                               change = FALSE)

      sem_sub <- run_origin_SEM(data = dat_sub,
                                landuse = "grazing")

      sem_sub_res <- summary(sem_sub)

      p4 <- make_SEM_figure(sem_results = sem_sub_res,
                            landuse = "grazing")

      #(p1 + p2) + plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine')))
      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine', 'c)', 'd)')))

    }
  ),

  tar_target(
    name = SEM_origin_change_fig,
    command = {

      # origin cutting
      dat_alp <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Alpine"),
                               landuse = "cutting",
                               diversity = log_ratio_diversity,
                               biomass = log_ratio_bio,
                               change = FALSE)

      sem_alp <- run_origin_SEM(data = dat_alp,
                                landuse = "cutting")

      sem_alp_res <- summary(sem_alp)

      p1 <- make_SEM_change_figure(sem_results = sem_alp_res,
                            landuse = "cutting")

      dat_sub <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Sub-alpine"),
                               landuse = "cutting",
                               diversity = log_ratio_diversity,
                               biomass = log_ratio_bio,
                               change = FALSE)

      sem_sub <- run_origin_SEM(data = dat_sub,
                                landuse = "cutting")

      sem_sub_res <- summary(sem_sub)

      p2 <- make_SEM_change_figure(sem_results = sem_sub_res,
                            landuse = "cutting")

      # origin grazing
      dat_alp2 <- prep_SEM_data(data = biomass_div |>
                                  filter(origSiteID == "Alpine"),
                                landuse = "grazing",
                                diversity = log_ratio_diversity,
                                biomass = log_ratio_bio,
                                change = FALSE)

      sem_alp2 <- run_origin_SEM(data = dat_alp2,
                                 landuse = "grazing")

      sem_alp_res2 <- summary(sem_alp2)

      p3 <- make_SEM_change_figure(sem_results = sem_alp_res2,
                            landuse = "grazing")

      dat_sub <- prep_SEM_data(data = biomass_div |>
                                 filter(origSiteID == "Sub-alpine"),
                               landuse = "grazing",
                               diversity = log_ratio_diversity,
                               biomass = log_ratio_bio,
                               change = FALSE)

      sem_sub <- run_origin_SEM(data = dat_sub,
                                landuse = "grazing")

      sem_sub_res <- summary(sem_sub)

      p4 <- make_SEM_change_figure(sem_results = sem_sub_res,
                            landuse = "grazing")

      #(p1 + p2) + plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine')))
      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine', 'c)', 'd)')))

    }
  ),


  tar_target(
    name = SEM_without_site_fig,
    command = {

      # origin cutting
      dat <- prep_SEM_data(data = biomass_div,
                               landuse = "cutting",
                               diversity = final_diversity,
                               biomass = final_bio,
                               change = FALSE)

      sem <- run_origin_SEM(data = dat,
                            landuse = "cutting")

      sem_res <- summary(sem)

      p1 <- make_SEM_figure(sem_results = sem_res,
                            landuse = "cutting")

      dat2 <- prep_SEM_data(data = biomass_div,
                           landuse = "cutting",
                           diversity = log_ratio_diversity,
                           biomass = log_ratio_bio,
                           change = FALSE)

      sem2 <- run_origin_SEM(data = dat2,
                            landuse = "cutting")

      sem_res2 <- summary(sem2)

      p2 <- make_SEM_change_figure(sem_results = sem_res2,
                            landuse = "cutting")

      # origin grazing
      dat3 <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = final_diversity,
                                biomass = final_bio,
                                change = FALSE)

      sem3 <- run_origin_SEM(data = dat3,
                                 landuse = "grazing")

      sem3 <- summary(sem3)

      p3 <- make_SEM_figure(sem_results = sem3,
                            landuse = "grazing")

      dat4 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio,
                            change = FALSE)

      sem4 <- run_origin_SEM(data = dat4,
                             landuse = "grazing")

      sem4 <- summary(sem4)

      p4 <- make_SEM_change_figure(sem_results = sem4,
                            landuse = "grazing")

      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Cutting (final)', 'b) change', 'c) Grazing (final)', 'd) change')))

    }
  ),



  tar_target(
    name = SEM_diversity_final_fig,
    command = {

      # richness cutting
      dat_rich <- prep_SEM_data(data = biomass_div,
                               landuse = "cutting",
                               diversity = final_richness,
                               biomass = final_bio,
                               change = FALSE)

      sem_rich <- run_origin_SEM(data = dat_rich,
                         landuse = "cutting")

      sem_rich_res <- summary(sem_rich)

      p1 <- make_SEM_figure(sem_results = sem_rich_res,
                            landuse = "cutting")

      # eveness
      dat_even <- prep_SEM_data(data = biomass_div,
                               landuse = "cutting",
                               diversity = final_evenness,
                               biomass = final_bio,
                               change = FALSE)

      sem_even <- run_origin_SEM(data = dat_even,
                         landuse = "cutting")

      sem_even_res <- summary(sem_even)

      p2 <- make_SEM_figure(sem_results = sem_even_res,
                            landuse = "cutting")

      # richness cutting
      dat_rich <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = final_richness,
                                biomass = final_bio,
                                change = FALSE)

      sem_rich <- run_origin_SEM(data = dat_rich,
                          landuse = "grazing")

      sem_rich_res <- summary(sem_rich)

      p3 <- make_SEM_figure(sem_results = sem_rich_res,
                            landuse = "grazing")

      # eveness
      dat_even <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = final_evenness,
                                biomass = final_bio,
                                change = FALSE)

      sem_even <- run_origin_SEM(data = dat_even,
                          landuse = "grazing")

      sem_even_res <- summary(sem_even)

      p4 <- make_SEM_figure(sem_results = sem_even_res,
                            landuse = "grazing")

      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Richness', 'b) Evenness')))

    }
  ),


  tar_target(
    name = SEM_diversity_change_fig,
    command = {

      # richness cutting
      dat_rich <- prep_SEM_data(data = biomass_div,
                                landuse = "cutting",
                                diversity = log_ratio_richness,
                                biomass = log_ratio_bio,
                                change = FALSE)

      sem_rich <- run_origin_SEM(data = dat_rich,
                          landuse = "cutting")

      sem_rich_res <- summary(sem_rich)

      p1 <- make_SEM_change_figure(sem_results = sem_rich_res,
                            landuse = "cutting")

      # eveness
      dat_even <- prep_SEM_data(data = biomass_div,
                                landuse = "cutting",
                                diversity = log_ratio_evenness,
                                biomass = log_ratio_bio,
                                change = FALSE)

      sem_even <- run_origin_SEM(data = dat_even,
                          landuse = "cutting")

      sem_even_res <- summary(sem_even)

      p2 <- make_SEM_change_figure(sem_results = sem_even_res,
                            landuse = "cutting")

      # richness cutting
      dat_rich <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = log_ratio_richness,
                                biomass = log_ratio_bio,
                                change = FALSE)

      sem_rich <- run_origin_SEM(data = dat_rich,
                          landuse = "grazing")

      sem_rich_res <- summary(sem_rich)

      p3 <- make_SEM_change_figure(sem_results = sem_rich_res,
                            landuse = "grazing")

      # eveness
      dat_even <- prep_SEM_data(data = biomass_div,
                                landuse = "grazing",
                                diversity = log_ratio_evenness,
                                biomass = log_ratio_bio,
                                change = FALSE)

      sem_even <- run_origin_SEM(data = dat_even,
                          landuse = "grazing")

      sem_even_res <- summary(sem_even)

      p4 <- make_SEM_change_figure(sem_results = sem_even_res,
                            landuse = "grazing")

      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Richness', 'b) Evenness')))

    }
  ),

  tar_target(
    name = SEM_origin_richness_final_fig,
    command = {

      # richness cutting
      dat_rich <- prep_SEM_data(data = biomass_div |>
                                  filter(origSiteID == "Alpine"),
                                landuse = "cutting",
                                diversity = final_richness,
                                biomass = final_bio,
                                change = FALSE)

      sem_rich <- run_origin_SEM(data = dat_rich,
                                 landuse = "cutting")

      sem_rich_res <- summary(sem_rich)

      p1 <- make_SEM_figure(sem_results = sem_rich_res,
                            landuse = "cutting")

      dat_rich2 <- prep_SEM_data(data = biomass_div |>
                                  filter(origSiteID == "Sub-alpine"),
                                landuse = "cutting",
                                diversity = final_richness,
                                biomass = final_bio,
                                change = FALSE)

      sem_rich2 <- run_origin_SEM(data = dat_rich2,
                                 landuse = "cutting")

      sem_rich_res2 <- summary(sem_rich2)

      p2 <- make_SEM_figure(sem_results = sem_rich_res2,
                            landuse = "cutting")

      # eveness
      dat_even <- prep_SEM_data(data = biomass_div |>
                                  filter(origSiteID == "Alpine"),
                                landuse = "cutting",
                                diversity = final_evenness,
                                biomass = final_bio,
                                change = FALSE)

      sem_even <- run_origin_SEM(data = dat_even,
                                 landuse = "cutting")

      sem_even_res <- summary(sem_even)

      p3 <- make_SEM_figure(sem_results = sem_even_res,
                            landuse = "cutting")

      dat_even2 <- prep_SEM_data(data = biomass_div |>
                                  filter(origSiteID == "Sub-alpine"),
                                landuse = "cutting",
                                diversity = final_evenness,
                                biomass = final_bio,
                                change = FALSE)

      sem_even2 <- run_origin_SEM(data = dat_even2,
                                 landuse = "cutting")

      sem_even_res2 <- summary(sem_even2)

      p4 <- make_SEM_figure(sem_results = sem_even_res2,
                            landuse = "cutting")

      (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = list(c('a) Rich alpine', 'b) sub-alpine', 'c) Even alpine', 'd) sub-alpine')))

    }
  )


)

