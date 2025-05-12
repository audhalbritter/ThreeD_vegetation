# trait analysis

trait_plan <- list(
  
  # join traits
  tar_target(
     name = joint_traits,
     command = bind_rows(trait_mean_all |>
                          mutate(status = "all"),
                        trait_mean_status |>
                          select(-data, -trait_impute) |> 
                          unnest(trait_mean))
   ),

   tar_target(name = traits,
              command = joint_traits |>
                filter(trait_trans %in% c("plant_height_cm_log",
                              "light",
                              "nutrients",
                              "moisture",
                              "temperature")) |>
                mutate(status = factor(status, levels = c("all", "extinction",
              "decrease", "stable", "increase", "colonization")),
              warming = factor(warming, levels = c("Ambient", "Warming"))) |>
                filter(!status %in% c("all", "stable")) |>
                filter(grazing != "Natural") |> 
                mutate(figure_names = factor(figure_names,
                  levels = c("Plant~height~(cm)",
                               "Light",
                               "Moisture",
                               "Temperature",
                               "Nutrients")))
                              ),

    tar_target(name = trait_model,
                command = traits |>
                  group_by(origSiteID, status2, trait_trans, figure_names) |>
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
                      Nitrogen_log = seq(min(.x$Nitrogen_log), 
                      max(.x$Nitrogen_log), length.out = 100)
                    )),
                    # Predict using the model on the new grid
                    predictions = map2(model, newdata, ~ {
                      pred <- predict(.x, newdata = .y, 
                        interval = "confidence", 
                        level = 0.95)
                      cbind(.y, as.data.frame(pred))  
                      # bind prediction + CI columns to newdata
                    }
                  )
                )
  ),

tar_target(
  name = trait_prediction,
  command = trait_model |>
    unnest(predictions) |>
    left_join(traits |>
      select(grazing_num, grazing) |>
      distinct(), by = "grazing_num")
),

tar_target(
  name = trait_prediction_clean,
  command = trait_prediction |> 
    select(-c(data:newdata))
),

tar_target(
  name = traits_stats,
  command = trait_model |>
    select(anova_tidy) |>
    unnest(anova_tidy) |>
    ungroup() |>
    mutate(term = str_replace(term, "Nitrogen_log", "N"),
           term = str_replace(term, "warming", "W"),
           term = str_replace(term, "grazing_num", "C"),
           term = str_replace_all(term, ":", "x"))
),

#large trait figure
  tar_target(
    name = traits_figure,
    command = make_trait_figure(traits, traits_stats, trait_prediction, col_palette)
  ),

  # small trait figure
  tar_target(
    name = small_traits_figure,
    command = make_trait_figure_small(traits, traits_stats, trait_prediction_clean, col_palette)
  )
  
  
)

# check significans
# trait_model |>
#   select(result) |>
#   unnest(result) |>
#   ungroup() |>
#   mutate(term = str_replace(term, "Nitrogen_log", "N"),
#          term = str_replace(term, "warmingWarming", "W"),
#          term = str_replace(term, "grazing_num", "C"),
#          term = str_replace_all(term, ":", "x")) |>
#   filter(p.value <= 0.05,
#          term != "(Intercept)")