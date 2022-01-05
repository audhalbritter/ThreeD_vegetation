
library(vegan)
library(broom)









# analyse
library(lme4)
library(broom.mixed)
library(performance)
library(broom)
diversity_data %>%
richness_data %>%
  group_by(origSiteID, grazing) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(delta ~ Namount_kg_ha_y * warming, data = .,)),
  #mutate(model = map(data, ~glmer(richness ~ Namount_kg_ha_y + (1|origBlockID), data = .)),
         result = map(model, tidy)) %>%
  unnest(result) %>% filter(p.value <= 0.05, term != "(Intercept)") %>% arrange(origSiteID, grazing)


purrr::map(
  .x = model_list,
  .f = ~ performance::check_model(.x))

dd <- diversity_data %>%
  filter(origSiteID == "Joa",
         grazing == "C")
fit <- lm(richness ~ Namount_kg_ha_y * warming, dd)
performance::check_model(fit)


diversity_data %>%
  group_by(origSiteID, grazing) %>%
  nest() %>%
  mutate(model = map(data, ~ glm(richness ~ Namount_kg_ha_y * warming, data = ., family = "poisson")),
         plot = map(fit, performance::check_model))



  group_by(origSiteID, grazing, functional_group) %>%
  nest() %>%
  mutate(model = map(data, ~lm(delta ~ Namount_kg_ha_y*warming, data = .)),
         result = map(model, tidy)) %>%
  unnest(result) %>% filter(term != "(Intercept)",
                            p.value <= 0.05) %>% arrange(origSiteID, functional_group, grazing) %>% select(term, estimate)
