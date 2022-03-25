# make analysis
analysis_plan <- list(

  # diversity
  tar_target(
    name = diversity_analysis,
    command = {
      diversity_data %>%
        group_by(origSiteID, grazing, diversity_index) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(delta ~ Nitrogen_log * warming, data = .,)),
               result = map(model, tidy)) %>%
        unnest(result) %>%
        arrange(origSiteID, grazing)
      #%>% filter(p.value <= 0.05, term != "(Intercept)")

    }),

  # cover
  tar_target(
    name = cover_analysis,
    command = {
      # log Nitrogen
      cover_data %>%
        filter(functional_group %in% c("forb", "graminoid")) |>
        group_by(origSiteID, grazing, functional_group) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(delta ~ Nitrogen_log * warming, data = .,)),
               result = map(model, tidy)) %>%
        unnest(result) %>%
        #filter(p.value <= 0.05, term != "(Intercept)") %>%
        arrange(origSiteID, grazing)
    })



)









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
