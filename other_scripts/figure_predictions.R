### MAKE FIGURES USING PREDICTIONS FROM THE MODEL

make_veg_figure <- function(dat, title, annotate, legend.position) {

  # grey and pink palette
  NxW_col_palette <- c("grey", "#CC79A7")

  ggplot(dat, aes(x = Nitrogen_log, y = delta, color = warming, linetype = grazing, shape = grazing)) +
    geom_point(size = 2) +
    geom_line(aes(y = .fitted)) +
    labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
         y = "Change in cover (2022 - 2019)",
         title = title) +
    scale_colour_manual(name = "Warming", values = NxW_col_palette) +
    scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
    scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5),
                     labels = c("0", "2.7", "7.4", "20.1", "54.6", "148.4")) +
    geom_text(aes(x = 0.5, y = 68, label = ann), data = annotation, inherit.aes = FALSE) +
    facet_grid(functional_group ~ origSiteID, scales = "free_y") +
    theme_bw() +
    theme(legend.position = legend.position,
          legend.box="vertical",
          text = element_text(size = 17))

}


# grazing intensity
# bind model output and data together
out <- bind_cols(cover_result |>
            select(-data, -model, -check, -tidy_result) |>
            unnest(fitted_result),

          cover_result |>
            unnest(data) |>
            ungroup() |>
            select(turfID:grazing_num, -Nitrogen_log, -warming, -grazing_num))


# annotation
annotation <- tibble(functional_group = c("forb", "graminoid", "forb", "graminoid"),
                     origSiteID = c("Alpine", "Alpine", "Sub-alpine", "Sub-alpine"),
                     ann = c("WxN", "GxW + WxN", "N", "G + W + N"))

make_veg_figure(out,
                title = "Grazing intensity",
                legend.position = "top")



# Natural grazing
out <- bind_cols(cover_cn_result |>
                   select(-data, -model, -check, -tidy_result) |>
                   unnest(fitted_result),

                 cover_cn_result |>
                   unnest(data) |>
                   ungroup() |>
                   select(turfID:Namount_kg_ha_y, -Nitrogen_log, -warming))


# annotation
annotation <- tibble(functional_group = c("forb", "graminoid", "forb", "graminoid"),
                     origSiteID = c("Alpine", "Alpine", "Sub-alpine", "Sub-alpine"),
                     ann = c("W + N", "WxN", "W", "N"))

make_veg_figure(out,
                title = "Natural grazing",
                legend.position = "top")




###********************************************************
# richness
# grazing intensity
out <- bind_cols(diversity_result |>
                   select(-data, -model, -check, -tidy_result) |>
                   unnest(fitted_result) |>
                   rename(Nitrogen_log_mod = Nitrogen_log, warming_mod = warming),

                 diversity_result |>
                   unnest(data) |>
                   ungroup() |>
                   select(warming:Nitrogen_log))




dat <- diversity |>
  filter(diversity_index == "richness",
         grazing != "Natural",
         origSiteID == "Alpine",
         !is.na(delta)) |>
  # make grazing numeric
  mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
         grazing_num = as.numeric(grazing_num))

fit <- lm(delta ~ Nitrogen_log * warming, data = dat)
summary(fit)
a_rich <- augment(fit, dat) %>%
  ggplot(aes(x = Nitrogen_log, y = delta, color = warming, linetype = grazing, shape = grazing)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted)) +
  labs(x = "",
       y = "Change in richness",
       title = "Alpine - intensity") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("dotted", "dashed", "solid")) +
  scale_shape_manual(name = "Grazing", values = c(1, 16, 17)) +
  annotate("text", x = 0.2, y = 10, label = "WxN", size = 5) +
  guides(colour = "none") +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size = 17))

dat <- diversity |>
  filter(diversity_index == "richness",
         grazing != "Natural",
         origSiteID == "Sub-alpine") |>
  # make grazing numeric
  mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
         grazing_num = as.numeric(grazing_num))

fit <- lm(delta ~ Nitrogen_log + warming, data = dat)
summary(fit)
suba_rich <- augment(fit, dat) %>%
  ggplot(aes(x = Nitrogen_log, y = delta, color = warming, linetype = grazing, shape = grazing)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted)) +
  labs(x = expression(log(Nitrogen~addition)~kg~ha^-1~y^-1),
       y = "Change in richness",
       title = "Sub-alpine") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("dotted", "dashed", "solid")) +
  scale_shape_manual(name = "Grazing", values = c(1, 16, 17)) +
  annotate("text", x = 0.2, y = 5, label = "N + W", size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 17))


# natural grazing
dat <- diversity |>
  filter(diversity_index == "richness",
         grazing %in% c("Natural", "Control"),
         !is.na(delta),
         origSiteID == "Alpine")

fit <- lm(delta ~ grazing + warming, data = dat)
summary(fit)
a_rich_nat <- augment(fit, dat) %>%
  ggplot(aes(x = Nitrogen_log, y = delta, color = warming, linetype = grazing, shape = grazing)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted)) +
  labs(x = "",
       y = "",
       title = "Alpine - natural grazing") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0)) +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size = 17))


dat <- diversity |>
  filter(diversity_index == "richness",
         grazing %in% c("Natural", "Control"),
         origSiteID == "Sub-alpine")

fit <- lm(delta ~ grazing + warming, data = dat)
summary(fit)
suba_rich_nat <- augment(fit, dat) %>%
  ggplot(aes(x = Nitrogen_log, y = delta, color = warming, linetype = grazing, shape = grazing)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted)) +
  labs(x = expression(log(Nitrogen~addition)~kg~ha^-1~y^-1),
       y = "",
       title = "Sub-alpine") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0)) +
  annotate("text", x = 0.2, y = 5, label = "W", size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 17))


richness <- (a_rich + a_rich_nat) / (suba_rich + suba_rich_nat)

ggsave(filename = "output/richness.png", richness, dpi = 300, width = 12, height = 8)
