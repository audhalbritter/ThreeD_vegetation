### Analysis

# # run three way interaction model with site
# run_full_model_v2 <- function(dat,
#                               group = NULL,
#                               response,
#                               grazing_var) {
#   # Check if grouping is provided
#   if (!is.null(group)) {
#     dat <- dat |>
#       group_by(across(all_of({{ group }})))
#   } else {
#     dat <- dat
#   }

#   # run linear, log and quadratic model
#   dat <- dat |>
#     rename(
#       .response = {{ response }},
#       .grazing = {{ grazing_var }}
#     ) |>
#     nest() |>
#     mutate(
#       model_linear = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y + origSiteID,
#         data = .
#       )),
#       model_log = map(data, ~ lm(.response ~ warming * .grazing * Nitrogen_log + origSiteID,
#         data = .
#       )),
#       model_quadratic = map(data, ~ lm(.response ~ warming * .grazing * poly(Namount_kg_ha_y, 2) + origSiteID,
#         data = .
#       )),

#       # get aic and r squared
#       glance_linear = map(
#         .x = model_linear,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_log = map(
#         .x = model_log,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_quadratic = map(
#         .x = model_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       )
#     )
# }



# run_main_model <- function(dat,
#                            group = NULL,
#                            response,
#                            grazing_var) {
#   # Check if grouping is provided
#   if (!is.null(group)) {
#     dat <- dat |>
#       group_by(across(all_of({{ group }})))
#   } else {
#     dat <- dat
#   }

#   # run linear, log and quadratic model
#   dat |>
#     rename(
#       .response = {{ response }},
#       .grazing = {{ grazing_var }}
#     ) |>
#     nest() |>
#     mutate(
#       # main effects model
#       model_main_linear = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y + origSiteID,
#         data = .
#       )),
#       model_main_log = map(data, ~ lm(.response ~ warming + .grazing + Nitrogen_log + origSiteID,
#         data = .
#       )),
#       model_main_quadratic = map(data, ~ lm(.response ~ warming + .grazing + poly(Namount_kg_ha_y, 2) + origSiteID,
#         data = .
#       )),
#       # get aic and r squared
#       glance_main_linear = map(
#         .x = model_main_linear,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_main_log = map(
#         .x = model_main_log,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_main_quadratic = map(
#         .x = model_main_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       )
#     )
# }


# run_full_model_v3 <- function(dat,
#                               group = NULL,
#                               response,
#                               grazing_var,
#                               biomass) {
#   # Check if grouping is provided
#   if (!is.null(group)) {
#     dat <- dat |>
#       group_by(across(all_of({{ group }})))
#   } else {
#     dat <- dat
#   }

#   # run linear, log and quadratic model
#   dat <- dat |>
#     rename(
#       .response = {{ response }},
#       .biomass = {{ biomass }},
#       .grazing = {{ grazing_var }}
#     ) |>
#     nest() |>
#     mutate(
#       model_bio_log = map(data, ~ lm(.response ~ log(.biomass) + origSiteID,
#         data = .
#       )),
#       model_main_linear = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y + origSiteID,
#         data = .
#       )),
#       model_biomain_linear = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y + log(.biomass) + origSiteID,
#         data = .
#       )),
#       model_interaction_linear = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y + origSiteID,
#         data = .
#       )),
#       model_biointeraction_linear = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y + log(.biomass) + origSiteID,
#         data = .
#       )),

#       # get aic and r squared
#       glance_bio_log = map(
#         .x = model_bio_log,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_main_linear = map(
#         .x = model_main_linear,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_biomain_linear = map(
#         .x = model_biomain_linear,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_interaction_linear = map(
#         .x = model_interaction_linear,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_biointeraction_linear = map(
#         .x = model_biointeraction_linear,
#         .f = ~ safely(glance)(.x)$result
#       )
#     )
# }



# run_full_model_v3_origin <- function(dat,
#                                      group = NULL,
#                                      response,
#                                      grazing_var,
#                                      biomass) {
#   # Check if grouping is provided
#   if (!is.null(group)) {
#     dat <- dat |>
#       group_by(across(all_of({{ group }})))
#   } else {
#     dat <- dat
#   }

#   # run linear, log and quadratic model
#   dat <- dat |>
#     rename(
#       .response = {{ response }},
#       .biomass = {{ biomass }},
#       .grazing = {{ grazing_var }}
#     ) |>
#     nest() |>
#     mutate(
#       model_bio_log = map(data, ~ lm(.response ~ log(.biomass),
#         data = .
#       )),
#       model_main_quadratic = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y,
#         data = .
#       )),
#       model_biomain_quadratic = map(data, ~ lm(.response ~ warming + .grazing + Namount_kg_ha_y + log(.biomass),
#         data = .
#       )),
#       model_interaction_quadratic = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y,
#         data = .
#       )),
#       model_biointeraction_quadratic = map(data, ~ lm(.response ~ warming * .grazing * Namount_kg_ha_y + log(.biomass),
#         data = .
#       )),

#       # get aic and r squared
#       glance_bio_log = map(
#         .x = model_bio_log,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_main_quadratic = map(
#         .x = model_main_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_biomain_quadratic = map(
#         .x = model_biomain_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_interaction_quadratic = map(
#         .x = model_interaction_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       ),
#       glance_biointeraction_quadratic = map(
#         .x = model_biointeraction_quadratic,
#         .f = ~ safely(glance)(.x)$result
#       )
#     )
# }


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

# make_prediction_v2 <- function(model, biomass = NULL) {
#   # make predictions for more data points
#   # Namount_kg_ha_y = seq(from = 0, to = 150, by = 0.5)
#   Namount_kg_ha_y <- seq(from = 0, to = 100, by = 0.5)
#   Nitrogen_log <- seq(from = 0, to = 5, by = 0.1)
#   final_bio <- seq(from = 2.05, to = 79, by = 0.5)

#   if (is.null(biomass)) {
#     model |>
#       mutate(
#         result = map(model, tidy),
#         anova = map(model, car::Anova),
#         anova_tidy = map(anova, tidy),
#         # make new data
#         newdata = ifelse(names == "log",
#           # for log model
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing, origSiteID) |>
#             crossing(Nitrogen_log)),
#           # for linear and quadratic models
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing, origSiteID) |>
#             crossing(Namount_kg_ha_y) |>
#             mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
#         ),
#         # predict with newdata
#         prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
#       )
#   } else {
#     model |>
#       mutate(
#         result = map(model, tidy),
#         anova = map(model, car::Anova),
#         anova_tidy = map(anova, tidy),
#         # make new data
#         newdata = ifelse(names == "log",
#           # for log model
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing, origSiteID) |>
#             crossing(Nitrogen_log, final_bio)),
#           # for linear and quadratic models
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing, origSiteID) |>
#             crossing(Namount_kg_ha_y, final_bio) |>
#             mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
#         ),
#         # predict with newdata
#         prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
#       )
#   }
# }



# make_prediction_v2_origin <- function(model,
#                                       biomass = NULL) {
#   # make predictions for more data points
#   # Namount_kg_ha_y = seq(from = 0, to = 150, by = 0.5)
#   Namount_kg_ha_y <- seq(from = 0, to = 100, by = 0.5)
#   Nitrogen_log <- seq(from = 0, to = 5, by = 0.1)
#   final_bio <- seq(from = 2.05, to = 79, by = 0.5)

#   if (is.null(biomass)) {
#     model |>
#       mutate(
#         result = map(model, tidy),
#         anova = map(model, car::Anova),
#         anova_tidy = map(anova, tidy),
#         # make new data
#         newdata = ifelse(names == "log",
#           # for log model
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing) |>
#             crossing(Nitrogen_log)),
#           # for linear and quadratic models
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing) |>
#             crossing(Namount_kg_ha_y) |>
#             mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
#         ),
#         # predict with newdata
#         prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
#       )
#   } else {
#     model |>
#       mutate(
#         result = map(model, tidy),
#         anova = map(model, car::Anova),
#         anova_tidy = map(anova, tidy),
#         # make new data
#         newdata = ifelse(names == "log",
#           # for log model
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing) |>
#             crossing(Nitrogen_log, final_bio)),
#           # for linear and quadratic models
#           map(.x = data, .f = ~ . |>
#             distinct(warming, grazing, .grazing) |>
#             crossing(Namount_kg_ha_y, final_bio) |>
#             mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
#         ),
#         # predict with newdata
#         prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence")
#       )
#   }
# }


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
