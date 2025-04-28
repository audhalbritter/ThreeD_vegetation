wide <- biomass_diversity |>
  mutate(warming_num = as.numeric(if_else(warming == "Ambient", 0, 1))) |>
  pivot_wider(names_from = diversity_index, values_from = delta, values_fill = 0)

library(corrplot)
M <-cor(wide |>
          select(Namount_kg_ha_y, warming_num, grazing_num, biomass, diversity, evenness, richness))
corrplot(M, method = 'number')


alp <- biomass_diversity |>
  filter(origSiteID == "Alpine",
         diversity_index == "diversity")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_q <- lm(delta ~ warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
fit_qb <- lm(delta ~ biomass + warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
anova(fit_q)
anova(fit_qb)
glance(fit_q)
glance(fit_qb)


alp <- biomass_diversity |>
  filter(origSiteID == "Alpine",
         diversity_index == "evenness")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_q <- lm(delta ~ warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
anova(fit_q)
anova(fit_qb)
glance(fit_q)
glance(fit_qb)


alp <- biomass_diversity |>
  filter(origSiteID == "Alpine",
         diversity_index == "richness")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_log <- lm(delta ~ warming * grazing_num * Nitrogen_log, data = alp)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = alp)
anova(fit_log)
anova(fit_logb)
glance(fit_log)
glance(fit_logb)



sub <- biomass_diversity |>
  filter(origSiteID == "Sub-alpine",
         diversity_index == "diversity")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = sub)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = sub)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = sub)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_q <- lm(delta ~ warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
anova(fit_q)
anova(fit_qb)


sub <- biomass_diversity |>
  filter(origSiteID == "Sub-alpine",
         diversity_index == "evenness")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = sub)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = sub)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = sub)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_q <- lm(delta ~ warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = alp)
anova(fit_q)
anova(fit_qb)


sub <- biomass_diversity |>
  filter(origSiteID == "Sub-alpine",
         diversity_index == "richness")

fit_lb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = sub)
fit_logb <- lm(delta ~ biomass * warming * grazing_num * Nitrogen_log, data = sub)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * poly(Namount_kg_ha_y, 2), data = sub)
glance(fit_lb)
glance(fit_logb)
glance(fit_qb)

fit_q <- lm(delta ~ warming * grazing_num * Namount_kg_ha_y, data = alp)
fit_qb <- lm(delta ~ biomass * warming * grazing_num * Namount_kg_ha_y, data = alp)
anova(fit_q)
anova(fit_qb)
