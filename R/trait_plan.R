# trait analysis

trait_plan <- list(

  # tar_target(
  #   name = trait_model_all,
  #   command = {
  #
  #     trait_model <- run_full_model(dat = trait_mean_all |>
  #                      filter(grazing != "Natural"),
  #                    group = c("origSiteID"),
  #                    response = mean,
  #                    grazing_var = grazing_num) |>
  #       # make long table
  #       pivot_longer(cols = -c(origSiteID, data),
  #                    names_sep = "_",
  #                    names_to = c(".value", "effects", "names")) |>
  #       unnest(glance) |>
  #       select(origSiteID:adj.r.squared, AIC, deviance)
  #   }
  # ),
  #
  # # only interaction model
  # tar_target(
  #   name = trait_model,
  #   command = trait_model_all |>
  #     # remove model with single effects
  #     filter(effects == "interaction",
  #            # choose log model
  #            names == "log") #|>
  #     # # select parsimonious model
  #     # filter(AIC == min(AIC))
  # ),
  #
  # tar_target(
  #   name = trait_output,
  #   command = make_prediction(trait_model)
  # ),
  #
  # # prepare model output
  # tar_target(
  #   name =   trait_prediction,
  #   command = trait_output |>
  #     # merge data and prediction
  #     mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
  #     select(origSiteID, output) |>
  #     unnest(output) |>
  #     rename(prediction = fit)
  # ),
  #
  # # stats
  # tar_target(
  #   name =   trait_anova_table,
  #   command = trait_output |>
  #     select(origSiteID, names, anova_tidy) |>
  #     unnest(anova_tidy) |>
  #     ungroup() |>
  #     fancy_stats()
  # ),
  #
  # # stats
  # tar_target(
  #   name =   trait_summary_table,
  #   command = trait_output |>
  #     select(origSiteID, names, result) |>
  #     unnest(result) |>
  #     ungroup() |>
  #     fancy_stats()
  # ),


  ### winners and loosers
  # tar_target(
  #   name = number_cover,
  #   command = cover_wl |>
  #     filter(grazing == "Control") |>
  #     group_by(turfID, warming, grazing, Namount_kg_ha_y, year, origSiteID, status) |>
  #     summarise(cover = sum(cover),
  #               n = n()) |>
  #     group_by(warming, grazing, Namount_kg_ha_y, year, origSiteID, status) |>
  #     summarise(n = mean(n),
  #               cover = mean(cover)) |>
  #     mutate(status = factor(status, levels = c("loser", "decrease", "stable", "increase", "winner")))
  # ),
  #
  # tar_target(
  #   name = win_loos_nc_figure,
  #   command = {
  #
  #     nr <- ggplot(number_cover,
  #            aes(x = as.factor(Namount_kg_ha_y), y = n,
  #                fill = warming)) +
  #       geom_col(position = "dodge") +
  #       scale_fill_manual(name = "Warming", values = col_palette) +
  #       labs(x = "Nitrogen addition", y = "Number of species") +
  #       facet_grid(cols = vars(status),
  #                  rows = vars(origSiteID)) +
  #       theme_bw()
  #
  #     cov <- ggplot(number_cover,
  #                   aes(x = as.factor(Namount_kg_ha_y), y = cover,
  #                       fill = warming)) +
  #       geom_col(position = "dodge") +
  #       scale_fill_manual(name = "Warming", values = col_palette) +
  #       labs(x = "Nitrogen addition", y = "Sum of cover (%)") +
  #       facet_grid(cols = vars(status),
  #                  rows = vars(origSiteID)) +
  #       theme_bw()
  #
  #     nr / cov + plot_layout(guides = "collect") &
  #       theme(text = element_text(size = 12))
  #
  #   }
  # )


)
