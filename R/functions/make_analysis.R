### Analysis

# run three way interaction model (quadratic term)
run_full_model <- function(dat, group, response, grazing_var){

  dat |>
    rename(.response = {{response}},
           .grazing = {{grazing_var}}) |>
    group_by(across(all_of({{group}})))|>
    nest() |>
    # run linear, log and quadratic model
    mutate(model_linear = map(data, ~lm(.response ~ warming * .grazing * Namount_kg_ha_y, data = .)),
           model_log = map(data, ~lm(.response ~ warming * .grazing * Nitrogen_log, data = .)),
           model_quadratic = map(data, ~lm(.response ~ warming * .grazing * poly(Namount_kg_ha_y, 2), data = .)),

           # get aic and r squared
           glance_linear = map(.x = model_linear, .f = ~ safely(glance)(.x)$result),
           glance_log = map(.x = model_log, .f = ~ safely(glance)(.x)$result),
           glance_quadratic = map(.x = model_quadratic, .f = ~ safely(glance)(.x)$result))

}


# 3 way interaction model with log transformed N
# run_model <- function(dat, group, response, grazing_var){
#
#   dat |>
#     rename(.response = {{response}},
#            .grazing = {{grazing_var}}) |>
#     group_by(across(all_of({{group}})))|>
#     nest() |>
#     mutate(model = map(data, ~lm(.response ~ warming * .grazing * Nitrogen_log, data = .)),
#            result = map(model, tidy),
#            prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"))
#
# }


make_prediction <- function(model){

  model |>
    mutate(result = map(model, tidy),
           prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"))


}




do_model_selection <- function(functional_group_cover){

  dat <- functional_group_cover |>
    filter(grazing != "Natural")

  alp_gram <- dat |>
    filter(origSiteID == "Alpine",
           functional_group == "graminoid")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = alp_gram,
            na.action = "na.fail")

  model_selection_1 <- dredge(fit, rank = "AICc", extra = "R^2")


  alp_forb <- dat |>
    filter(origSiteID == "Alpine",
           functional_group == "forb")

  fit2 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = alp_forb,
             na.action = "na.fail")

  model_selection_2 <- dredge(fit2, rank = "AICc", extra = "R^2")


  sub_gram <- dat |>
    filter(origSiteID == "Sub-alpine",
           functional_group == "graminoid")

  fit3 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = sub_gram,
             na.action = "na.fail")

  model_selection_3 <- dredge(fit3, rank = "AICc", extra = "R^2")


  sub_forb <- dat |>
    filter(origSiteID == "Sub-alpine",
           functional_group == "forb")

  fit4 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = sub_forb,
             na.action = "na.fail")

  model_selection_4 <- dredge(fit4, rank = "AICc", extra = "R^2")


  model_selection_output <- bind_rows(
    Alpine_graminoid = model_selection_1 |> as_tibble(),
    Alpine_forb = model_selection_2 |> as_tibble(),
    Subalpine_graminoid = model_selection_3 |> as_tibble(),
    Subalpine_forb = model_selection_4 |> as_tibble(),
    .id = "origSiteID_fungroup") |>
    separate(col = origSiteID_fungroup, into = c("origSiteID", "functional_group"), sep = "_") |>
    # select for models within dAIC 2
    filter(delta < 2) |>
    rename(Intercept = `(Intercept)`, G = grazing_num, N = Nitrogen_log, W = warming,
           GxN = `grazing_num:Nitrogen_log`, GxW = `grazing_num:warming`,
           NxW = `Nitrogen_log:warming`, GxNxW = `grazing_num:Nitrogen_log:warming`)


  return(model_selection_output)

}


run_best_models <- function(dat){

  cover_result <- dat |>
  nest() |>
  mutate(model = map(data, ~lm(formula = best_model, data = .)),
         check = map(model, check_model),
         tidy_result = map(model, tidy),
         fitted_result = map(model, augment))

  return(cover_result)

}











#
# purrr::map(
#   .x = model_list,
#   .f = ~ performance::check_model(.x))
#
# dd <- diversity_data %>%
#   filter(origSiteID == "Joa",
#          grazing == "C")
# fit <- lm(richness ~ log(Namount_kg_ha_y) * warming, dd)
# performance::check_model(fit)
#
#
# diversity_data %>%
#   group_by(origSiteID, grazing) %>%
#   nest() %>%
#   mutate(model = map(data, ~ glm(richness ~ Namount_kg_ha_y * warming, data = ., family = "poisson")),
#          plot = map(fit, performance::check_model))


# library(palmerpenguins)
# penguins |>
#   filter(!is.na(sex)) |>
#   group_by(species) |>
#   nest() |>
#   mutate(model = map(data, ~ lm(bill_length_mm ~ body_mass_g * sex, data = ., na.action = "na.fail")),
#          mod_selection = map(model, ~dredge(model)))
#
#
# penguins |>
#   filter(!is.na(sex)) |>
#   # run different models per group
#   mutate(sp_formula = case_when(species == "Adelie" ~ "bill_length_mm ~ body_mass_g * sex",
#                                 species == "Gentoo" ~ "bill_length_mm ~ body_mass_g + sex",
#                              TRUE ~ "bill_length_mm ~ 1")) |>
#   group_by(species, sp_formula) |>
#   nest() |>
#   mutate(model = map(data, ~ lm(formula = sp_formula, data = .)),
#          check = performance(model),
#          result = map(model, tidy)) |>
#   unnest(result)

