

joint_traits <- bind_rows(trait_mean_all |>
                        mutate(status = "all"),
                      trait_mean_status |>
                        unnest(trait_mean))

write_csv(joint_traits, "joint_traits.csv")


traits_raw <- read_csv("joint_traits.csv")

traits <- traits_raw |>
  filter(trait_trans %in% c("plant_height_cm_log",
                                "light",
                                "nutrients",
                                "moisture",
                                "temperature")) |>
      mutate(status = factor(status, levels = c("all", "loser", "decrease", "stable", "increase", "winner"))) |>
  filter(!status %in% c("all", "stable")) |>
  filter(grazing != "Natural")

trait_model <- run_full_model(dat = traits,
                                 group = c("trait_trans", "status", "origSiteID"),
                                 response = mean,
                                 grazing_var = grazing_num) |>
  # make long table
  pivot_longer(cols = -c(origSiteID, trait_trans, status, data),
               names_sep = "_",
               names_to = c(".value", "mod", "names")) |>
  unnest(glance) |>
  filter(mod == "interaction") |>
  select(origSiteID:adj.r.squared, AIC) |>
  # choosing log model
  filter(names == "log")

trait_output <- make_prediction_v2_origin(trait_model)

trait_prediction <- trait_output |>
  # merge data and prediction
  mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
  select(output) |>
  unnest(output) |>
  rename(prediction = fit)


traits_stats <- trait_output |>
  filter(!status %in% c("all", "stable")) |>
  select(names, anova_tidy) |>
  unnest(anova_tidy) |>
  ungroup() |>
  fancy_stats()


# trait_output |>
#   filter(!status %in% c("all", "stable")) |>
#   unnest(anova_tidy) |>
#   filter(p.value <= 0.05) |>
#   select(term:p.value) |>
#   arrange(origSiteID, status) |>
#   mutate(term = str_replace(term, "Nitrogen_log", "N"),
#          term = str_replace(term, "warming", "W"),
#          term = str_replace(term, "grazing_num", "C"),
#          term = str_replace_all(term, ":", "x"))



#figure_traits <-
traits |>
    ggplot(aes(x = Nitrogen_log, y = mean,
               colour = warming, shape = grazing,
               linetype = grazing)) +
      geom_point() +
      geom_line(data = trait_prediction,
                aes(x = Nitrogen_log, y = prediction,
                    colour = warming, linetype = grazing,
                    group = interaction(warming, grazing)),
                linewidth = 0.5) +
      scale_colour_manual(name = "Warming", values = col_palette) +
      scale_shape_manual(name = "Grazing", values = c(17, 1, 2)) +
      scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
      #scale_alpha_manual(values = c(1, 0.5)) +
      labs(y = "Trait mean",
           x = "Log(Nitrogen)") +
      facet_grid(rows = vars(figure_names),
                 #cols = vars(status),
                 cols = vars(status, origSiteID),
                 scales = "free_y", labeller = label_parsed) +
      theme_bw()



traits2 <- traits |>
  mutate(figure_names = factor(figure_names,
                             levels = c("Plant~height~(cm)",
                                        "Light",
                                        "Moisture",
                                        "Temperature",
                                        "Nutrients")))

trait_model <- traits2 |>
  group_by(origSiteID, status, trait_trans, figure_names) |>
  nest() |>
  # run model
  mutate(
    model = map(data, ~ lm(mean ~ warming * grazing_num * Nitrogen_log, data = .x)),

    result = map(model, tidy),
    anova = map(model, car::Anova),
    anova_tidy = map(anova, tidy)) |>
  # Create a prediction grid per group
  mutate(
    newdata = map(data, ~ expand.grid(
      warming = unique(.x$warming),
      grazing_num = unique(.x$grazing_num),
      Nitrogen_log = seq(min(.x$Nitrogen_log), max(.x$Nitrogen_log), length.out = 100)
    )),

    # Predict using the model on the new grid
    predictions = map2(model, newdata, ~ {
      pred <- predict(.x, newdata = .y, interval = "confidence", level = 0.95)
      cbind(.y, as.data.frame(pred))  # bind prediction + CI columns to newdata
    }),
  )

pred <- trait_model |>
  unnest(predictions) |>
  left_join(traits |>
              select(grazing_num, grazing) |>
              distinct(), by = "grazing_num")


traits_stats <- trait_model |>
  select(anova_tidy) |>
  unnest(anova_tidy) |>
  ungroup() |>
  mutate(term = str_replace(term, "Nitrogen_log", "N"),
         term = str_replace(term, "warming", "W"),
         term = str_replace(term, "grazing_num", "C"),
         term = str_replace_all(term, ":", "x"))

trait_model |>
  select(result) |>
  unnest(result) |>
  ungroup() |>
  mutate(term = str_replace(term, "Nitrogen_log", "N"),
         term = str_replace(term, "warmingWarming", "W"),
         term = str_replace(term, "grazing_num", "C"),
         term = str_replace_all(term, ":", "x")) |>
  filter(p.value <= 0.05,
         term != "(Intercept)")


max_y <- traits2 |>
  group_by(figure_names) |>
  summarise(y_max = max(mean, na.rm = TRUE), .groups = "drop")


traits_text <- traits_stats |>
  filter(p.value <= 0.05) |>
  mutate(nr = 1:n(), .by = c(origSiteID, status, trait_trans, figure_names)) |>
  left_join(max_y, by = c("figure_names")) |>
  mutate(x = if_else(origSiteID == "Alpine", -Inf, Inf),
         y = y_max + nr * 0.1 * y_max,
         hjust = if_else(origSiteID == "Alpine", 0, 1))

figure_traits <- traits2 |>
  ggplot(aes(x = Nitrogen_log, y = mean,
             colour = warming, shape = grazing,
             linetype = grazing, alpha = origSiteID)) +
  geom_point() +
  geom_line(data = pred,
            aes(y = fit, alpha = origSiteID),
            linewidth = 0.5) +
  geom_text(data = traits_text,
            aes(x = x, y = y, label = term,
                hjust = hjust,
                alpha = origSiteID),
            inherit.aes = FALSE,
            size = 3) +
  scale_colour_manual(name = "Warming", values = col_palette) +
  scale_shape_manual(name = "Clipping", values = c(17, 1, 2)) +
  scale_linetype_manual(name = "Clipping", values = c("solid", "dashed", "dotted")) +
  scale_alpha_manual(values = c(1, 0.6)) +
  labs(y = "Trait mean",
       x = "Log(Nitrogen)") +
  facet_grid(rows = vars(figure_names),
             cols = vars(status),
             scales = "free_y", labeller = label_parsed) +
  theme_bw()



ggsave(filename = "Fig3_traits.jpeg", figure_traits,
       width = 10, height = 10,
       dpi = 300, device = "jpeg",
       path = here("manuscript/figures"),
       limitsize = FALSE)

niche <- read_csv(file = "ClimDiff_LynnETAL_data.csv")
