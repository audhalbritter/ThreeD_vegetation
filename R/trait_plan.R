# trait analysis

trait_plan <- list(
  
  # join traits
#   tar_target(
#      name = joint_traits,
#      command = bind_rows(trait_mean_all |>
#                           mutate(status = "all"),
#                         trait_mean_status |>
#                           select(-data, -trait_impute) |> 
#                           unnest(trait_mean))
#    ),

#    tar_target(name = traits,
#               command = joint_traits |>
#                 filter(trait_trans %in% c("plant_height_cm_log",
#                               "light",
#                               "nutrients",
#                               "moisture",
#                               "temperature")) |>
#                 mutate(status = factor(status, levels = c("all", "extinction",
#               "decrease", "stable", "increase", "colonization")),
#               warming = factor(warming, levels = c("Ambient", "Warming"))) |>
#                 filter(!status %in% c("all", "stable")) |>
#                 filter(grazing != "Natural") |> 
#                 mutate(figure_names = factor(figure_names,
#                   levels = c("Plant~height~(cm)",
#                                "Light",
#                                "Moisture",
#                                "Temperature",
#                                "Nutrients")))
#                               ),

#     tar_target(name = trait_model,
#                 command = traits |>
#                   group_by(origSiteID, status2, trait_trans, figure_names) |>
#                   nest() |>
#                   # run model
#                   mutate(
#                     model = map(data, ~ lm(mean ~ warming * grazing_num * Nitrogen_log, data = .x)),
                    
#                     result = map(model, tidy),
#                     anova = map(model, car::Anova),
#                     anova_tidy = map(anova, tidy)) |>
#                   # Create a prediction grid per group
#                   mutate(
#                     newdata = map(data, ~ expand.grid(
#                       warming = unique(.x$warming),
#                       grazing_num = unique(.x$grazing_num),
#                       Nitrogen_log = seq(min(.x$Nitrogen_log), 
#                       max(.x$Nitrogen_log), length.out = 100)
#                     )),
#                     # Predict using the model on the new grid
#                     predictions = map2(model, newdata, ~ {
#                       pred <- predict(.x, newdata = .y, 
#                         interval = "confidence", 
#                         level = 0.95)
#                       cbind(.y, as.data.frame(pred))  
#                       # bind prediction + CI columns to newdata
#                     }
#                   )
#                 )
#   ),

# tar_target(
#   name = trait_prediction,
#   command = trait_model |>
#     unnest(predictions) |>
#     left_join(traits |>
#       select(grazing_num, grazing) |>
#       distinct(), by = "grazing_num")
# ),

# tar_target(
#   name = trait_prediction_clean,
#   command = trait_prediction |> 
#     select(-c(data:newdata))
# ),

# tar_target(
#   name = traits_stats,
#   command = trait_model |>
#     select(anova_tidy) |>
#     unnest(anova_tidy) |>
#     ungroup() |>
#     mutate(term = str_replace(term, "Nitrogen_log", "N"),
#            term = str_replace(term, "warming", "W"),
#            term = str_replace(term, "grazing_num", "C"),
#            term = str_replace_all(term, ":", "x"))
# ),

#large trait figure
  # tar_target(
  #   name = traits_figure,
  #   command = make_trait_figure(traits, traits_stats, trait_prediction, col_palette)
  # ),

  # # small trait figure
  # tar_target(
  #   name = small_traits_figure,
  #   command = make_trait_figure_small(traits, traits_stats, trait_prediction_clean, col_palette)
  # ),

  #   # grazing trait figure
  #   tar_target(
  #     name = grazing_traits_figure,
  #     command = make_trait_figure_grazing(traits, traits_stats, trait_prediction_clean, col_palette)
  #   ),

    # Trats ridgeline plot
    # warming
    tar_target(
      name = traits_warming_plot,
      command = make_trait_ridgeline_plot(trait_mean_all |> 
                                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "salinity"),
                                            grazing != "Natural"), 
                                            group_var = "warming",
                                            custom_colors = treatment_palette[c(1, 2)],
                                            y_axis_label = "")
    ),

    tar_target(
      name = traits_nitrogen_plot,
      command = make_trait_ridgeline_plot(trait_mean_all |> 
                                            filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction", "salinity"),
                                            grazing != "Natural") |>
                                            mutate(Namount_kg_ha_y2 = as.factor(Namount_kg_ha_y)), 
                                            group_var = "Namount_kg_ha_y2",
                                            custom_colors = met.brewer(name="VanGogh3", n=7, type="discrete"),
                                            y_axis_label = "Nitrogen")
    ),

    # clipping
    tar_target(
      name = traits_clipping_plot,
      command = make_trait_ridgeline_plot(trait_mean_all |> 
                                            filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "salinity"),
                                            grazing != "Natural"), 
                                            group_var = "grazing",
                                            custom_colors = met.brewer(name="Manet", n=3, type="discrete"),
                                            y_axis_label = "Clipping")
    ),

    # biomass
    tar_target(
      name = traits_biomass_plot,
      command = make_trait_ridgeline_plot(trait_mean_all |>
                                            filter(trait_trans %in% c("plant_height_cm_log", "temperature",  "light", "moisture", "nutrients", "reaction", "salinity"),
                                            grazing != "Natural") |>
                                            tidylog::left_join(standing_biomass_back |> 
                                            filter(year == 2022,
                                            grazing != "Natural") |>
                                            mutate(biomass_log = log(standing_biomass)) |>
                                            select(-year),
                                            by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")), 
                                            group_var = "biomass_log",
                                            custom_colors = met.brewer(name="VanGogh3", n=7, type="discrete"),
                                            y_axis_label = "Log(Standing biomass)")
    ),

  # Trait statistical analysis
  tar_target(
    name = trait_statistical_analysis,
    command = {
      test_treatment_effects(data = trait_mean_all, biomass_data = standing_biomass_back)
    }
  ),

  tar_target(
    name = trait_stats_table,
    command = make_trait_stats(trait_statistical_analysis)
  )

)


