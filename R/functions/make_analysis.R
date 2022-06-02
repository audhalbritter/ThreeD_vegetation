### Analysis


# dat <- functional_group_cover |>
#   mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = "2"),
#          grazing_num = as.numeric(grazing_num)) |>
#   filter(functional_group %in% c("graminoid", "forb"))
#
#
# dat |>
#   group_by(origSiteID, functional_group) |>
#   nest() |>
#   mutate(model = map(data, ~ lm(delta ~ Nitrogen_log * warming * grazing_num, data = ., na.action = "na.fail")),
#          model_selection = map(data, ~ dredge(model)))

do_model_selection <- function(functional_group_cover){

  dat <- functional_group_cover |>
    filter(grazing != "Natural") |>
    # make grazing numeric
    mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
           grazing_num = as.numeric(grazing_num))


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
         result = map(model, tidy)) |>
  unnest(result)

  return(cover_result)

}


### MODEL SELECTION FOR C AND N GRAZING TREATMENTS
do_cn_model_selection <- function(functional_group_cover){

  # filter cn
  dat <- functional_group_cover |>
    filter(grazing %in% c("Control", "Natural"))

  # ALPINE
  alp_gram <- dat |>
    filter(origSiteID == "Alpine",
           functional_group == "graminoid")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing, data = alp_gram,
            na.action = "na.fail")

  model_selection_1 <- dredge(fit, rank = "AICc", extra = "R^2")


  alp_forb <- dat |>
    filter(origSiteID == "Alpine",
           functional_group == "forb")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing, data = alp_forb,
            na.action = "na.fail")

  model_selection_2 <- dredge(fit, rank = "AICc", extra = "R^2")

  # SUB-ALPINE
  sub_gram <- dat |>
    filter(origSiteID == "Sub-alpine",
           functional_group == "graminoid")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing, data = sub_gram,
            na.action = "na.fail")

  model_selection_3 <- dredge(fit, rank = "AICc", extra = "R^2")

  sub_forb <- dat |>
    filter(origSiteID == "Sub-alpine",
           functional_group == "forb")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing, data = sub_forb,
            na.action = "na.fail")

  model_selection_4 <- dredge(fit, rank = "AICc", extra = "R^2")

  # bind together
  model_cn_selection_output <- bind_rows(
    Alpine_graminoid = model_selection_1 |> as_tibble(),
    Alpine_forb = model_selection_2 |> as_tibble(),
    Subalpine_graminoid = model_selection_3 |> as_tibble(),
    Subalpine_forb = model_selection_4 |> as_tibble(),
    .id = "origSiteID_fungroup") |>
    separate(col = origSiteID_fungroup, into = c("origSiteID", "functional_group"), sep = "_") |>
    # select for models within dAIC 2
    filter(delta < 2) |>
    rename(Intercept = `(Intercept)`, G = grazing, N = Nitrogen_log, W = warming,
           GxN = `grazing:Nitrogen_log`, GxW = `grazing:warming`,
           NxW = `Nitrogen_log:warming`, GxNxW = `grazing:Nitrogen_log:warming`)


  return(model_cn_selection_output)
}



do_diversity_model_selection <- function(diversity){

  dat <- diversity |>
    filter(grazing != "Natural") |>
    # make grazing numeric
    mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
           grazing_num = as.numeric(grazing_num))


  # rich
  alp <- dat |>
    filter(origSiteID == "Alpine",
           diversity_index == "richness")

  fit <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = alp,
            na.action = "na.fail")

  model_selection_1 <- dredge(fit, rank = "AICc", extra = "R^2")


  sub <- dat |>
    filter(origSiteID == "Sub-alpine",
           diversity_index == "richness")

  fit_2 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = sub,
              na.action = "na.fail")

  model_selection_2 <- dredge(fit_2, rank = "AICc", extra = "R^2")

  # div
  alp <- dat |>
    filter(origSiteID == "Alpine",
           diversity_index == "diversity")

  fit_3 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = alp,
              na.action = "na.fail")

  model_selection_3 <- dredge(fit_3, rank = "AICc", extra = "R^2")


  sub <- dat |>
    filter(origSiteID == "Sub-alpine",
           diversity_index == "diversity")

  fit_4 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = sub,
              na.action = "na.fail")

  model_selection_4 <- dredge(fit_4, rank = "AICc", extra = "R^2")


  # even
  alp <- dat |>
    filter(origSiteID == "Alpine",
           diversity_index == "evenness")

  fit_5 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = alp,
              na.action = "na.fail")

  model_selection_5 <- dredge(fit_5, rank = "AICc", extra = "R^2")


  sub <- dat |>
    filter(origSiteID == "Sub-alpine",
           diversity_index == "evenness")

  fit_6 <- lm(delta ~ Nitrogen_log * warming * grazing_num, data = sub,
              na.action = "na.fail")

  model_selection_6 <- dredge(fit_6, rank = "AICc", extra = "R^2")


}







# NMDS ordination
# tar_target(
#   name = NMDS_ordination,
#   command = {
#     cover_wide <- cover |>
#       filter(year != 2020) |>
#       mutate(presence = 1) |>
#       distinct() |>
#       pivot_wider(names_from = species,
#                   values_from = presence,
#                   values_fill = 0)
#
#     sp_wide <- cover_wide |>
#       select(-c(origSiteID:Nitrogen_log))
#
#     NMDS_1 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 1)
#     NMDS_2 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 2)
#     NMDS_3 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 3)
#     NMDS_4 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 4)
#     NMDS_5 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 5)
#     NMDS_6 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 6)
#
#     stress_plot <- tibble(
#       stress = c(NMDS_1$stress, NMDS_2$stress, NMDS_3$stress, NMDS_4$stress, NMDS_5$stress, NMDS_6$stress),
#       dimensions = c(1:6)) %>%
#       ggplot(aes(x = dimensions, y = stress)) +
#       geom_point()
#
#     return(stress_plot)
#
#   })




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

