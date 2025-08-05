### Analysis

# run three way interaction model (quadratic term)
run_full_model <- function(dat, group, response, grazing_var) {
  dat |>
    rename(
      .response = {{ response }},
      .grazing = {{ grazing_var }}
    ) |>
    group_by(across(all_of({{ group }}))) |>
    nest() |>
    # run linear, log and quadratic model with interactions and only single effects
    mutate(
      model_interaction_linear = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y, data = .)),
      model_single_linear = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y, data = .)),
      model_interaction_log = map(data, ~ lm(.response ~ warming * .grazing * Nitrogen_log, data = .)),
      model_single_log = map(data, ~ lm(.response ~ warming + .grazing + Nitrogen_log, data = .)),
      model_interaction_quadratic = map(data, ~ lm(.response ~ warming * .grazing * poly(Namount_kg_ha_y, 2), data = .)),
      model_single_quadratic = map(data, ~ lm(.response ~ warming + .grazing + poly(Namount_kg_ha_y, 2), data = .)),

      # get aic and r squared
      glance_interaction_linear = map(.x = model_interaction_linear, .f = ~ safely(glance)(.x)$result),
      glance_single_linear = map(.x = model_single_linear, .f = ~ safely(glance)(.x)$result),
      glance_interaction_log = map(.x = model_interaction_log, .f = ~ safely(glance)(.x)$result),
      glance_single_log = map(.x = model_single_log, .f = ~ safely(glance)(.x)$result),
      glance_interaction_quadratic = map(.x = model_interaction_quadratic, .f = ~ safely(glance)(.x)$result),
      glance_single_quadratic = map(.x = model_single_quadratic, .f = ~ safely(glance)(.x)$result)
    )
}


make_prediction <- function(model) {
  # model |>
  #   mutate(result = map(model, tidy),
  #          prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"))

  # make predictions for more data points
  # Namount_kg_ha_y = seq(from = 0, to = 150, by = 0.5)
  Namount_kg_ha_y <- seq(from = 0, to = 100, by = 0.5)
  Nitrogen_log <- seq(from = 0, to = 5, by = 0.1)

  model |>
    mutate(
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy),
      # make new data
      newdata = ifelse(names == "log",
        # for log model
        map(.x = data, .f = ~ . |>
          distinct(warming, grazing, .grazing) |>
          crossing(Nitrogen_log)),
        # for linear and quadratic models
        map(.x = data, .f = ~ . |>
          distinct(warming, grazing, .grazing) |>
          crossing(Namount_kg_ha_y) |>
          mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
      ),
      # predict with newdata
      prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
    )
}

# CN prediction
make_CN_prediction <- function(model) {
  # make predictions for more data points
  # Namount_kg_ha_y = seq(from = 0, to = 150, by = 0.5)
  Namount_kg_ha_y <- seq(from = 0, to = 100, by = 0.5)
  Nitrogen_log <- seq(from = 0, to = 5, by = 0.1)
  model |>
    mutate(
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy),
      # make new data
      newdata = ifelse(names == "log",
        # for log model
        map(.x = data, .f = ~ . |>
          distinct(warming, .grazing) |>
          crossing(Nitrogen_log)),
        # for linear and quadratic models
        map(.x = data, .f = ~ . |>
          distinct(warming, .grazing) |>
          crossing(Namount_kg_ha_y) |>
          mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
      ),
      # predict with newdata
      prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
    )
}


# model checking
# purrr::map(
#   .x = model_list,
#   .f = ~ performance::check_model(.x))

# purrr::map(
#   .x = productivity_model$model,
#   .f = ~ performance::check_model(.x))
#
# purrr::map(
#   .x = cover_model$model,
#   .f = ~ performance::check_model(.x))
#
# purrr::map(
#   .x = diversity_model$model,
#   .f = ~ performance::check_model(.x))
#
#
# dat <- diversity |>
#   filter(origSiteID == "Alpine", diversity_index == "richness") |>
#   mutate(delta_log = log(delta + 150))
#
# fit <- lm(delta ~ warming * grazing_num * Nitrogen_log, dat)
# summary(fit)
# fit_l <- lm(delta_log ~ warming * grazing_num * Nitrogen_log, dat)
# summary(fit_l)
# check_model(fit_l)
#
# dat <- functional_group_cover |>
#   filter(origSiteID == "Sub-alpine", functional_group == "forb") |>
#   mutate(delta_log = log(delta + 150))
# hist(dat$delta_log)
# fit <- lm(delta ~ warming * grazing_num * Nitrogen_log, dat)
# summary(fit)
# fit_l <- lm(delta_log ~ warming * grazing_num * Nitrogen_log, dat)
# summary(fit_l)
# check_model(fit_l)


# Test treatment effects on traits
test_treatment_effects <- function(data, biomass_data, 
traits = c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction")) {
  
  # Filter data for specified traits
  trait_data <- data |>
    filter(grazing != "Natural") |>
    filter(trait_trans %in% traits)
  
  # Test warming effects for each trait
  results_warming <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        # Ensure warming is a factor
        dat <- .x |> mutate(warming = as.factor(warming))
        lm(mean ~ warming, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test nitrogen effects for each trait
  results_nitrogen <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ Nitrogen_log, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test grazing effects for each trait (excluding Natural)
  results_grazing <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ grazing_num, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test biomass effects for each trait (excluding Natural)
  results_biomass <- trait_data |>
    tidylog::left_join(
      biomass_data |> 
        filter(year == 2022, grazing != "Natural") |>
        mutate(biomass_log = log(standing_biomass)) |>
        select(-year),
      by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")
    ) |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ biomass_log, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Combine results
  results <- bind_rows(
    warming = results_warming, 
    nitrogen = results_nitrogen,
    grazing = results_grazing,
    biomass = results_biomass,
    .id = "treatment"
  ) |>
    select(-data, -model, -anova)

  return(results)
}

