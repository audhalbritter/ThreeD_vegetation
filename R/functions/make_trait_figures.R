# Make trait figures

make_trait_figure <- function(traits, traits_stats, trait_prediction, col_palette){

  max_y <- traits |>
  group_by(figure_names) |>
  summarise(y_max = max(mean, na.rm = TRUE), .groups = "drop")


traits_text <- traits_stats |>
  filter(p.value <= 0.05) |>
  mutate(nr = 1:n(), .by = c(origSiteID, status2, trait_trans, figure_names)) |>
  left_join(max_y, by = c("figure_names")) |>
  mutate(x = if_else(origSiteID == "Alpine", -Inf, Inf),
         y = y_max + nr * 0.1 * y_max,
         hjust = if_else(origSiteID == "Alpine", 0, 1))

traits |>
  ggplot(aes(x = Nitrogen_log, y = mean,
             colour = warming, shape = grazing,
             linetype = grazing, alpha = origSiteID)) +
  geom_point() +
  geom_line(data = trait_prediction,
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
             cols = vars(status2),
             scales = "free_y", labeller = label_parsed) +
  theme_bw()
  
}

make_trait_figure_small <- function(traits, traits_stats, trait_prediction_clean, col_palette){

# average for winners and losers
traits_wl <- traits |> 
  filter(trait_trans %in% c("plant_height_cm_log", "temperature")) |>
  group_by(origSiteID, status2, trait_trans, figure_names, Nitrogen_log, warming, grazing) |> 
  summarise(mean = mean(mean))
  
prediction <- trait_prediction_clean |> 
  filter(trait_trans %in% c("plant_height_cm_log", "temperature")) |>
    group_by(origSiteID, status2, trait_trans, figure_names, warming, Nitrogen_log) |> 
  summarise(fit = mean(fit))
  
# figure text
max_y <- traits_wl |>
  group_by(figure_names) |>
  summarise(y_max = max(mean, na.rm = TRUE), .groups = "drop")


traits_text <- traits_stats |>
  filter(p.value <= 0.05) |>
  filter(trait_trans %in% c("plant_height_cm_log", "temperature")) |> 
  mutate(nr = 1:n(), .by = c(origSiteID, status2, trait_trans, figure_names)) |>
  left_join(max_y, by = c("figure_names")) |>
  mutate(x = if_else(origSiteID == "Alpine", -Inf, Inf),
         y = y_max + nr * 0.05 * y_max,
         hjust = if_else(origSiteID == "Alpine", 0, 1))


traits_wl |> 
  ggplot(aes(
    x = Nitrogen_log, 
    y = mean
  )) +
  
  # Points: Warming colors (only for NxW, but also visible for N as grey)
  geom_point(aes(color = warming,
                 shape = grazing,
                 fill = interaction(origSiteID, warming))) +
  
  geom_line(data = prediction,
    aes(y = fit,
        colour = warming,
        linetype = origSiteID),
        linewidth = 0.5) +

  geom_text(
    data = traits_text,
    aes(x = x, y = y, label = term,
        hjust = hjust,
        alpha = origSiteID),
        inherit.aes = FALSE,
        size = 3
      ) +
  
    # Define color mapping
    scale_colour_manual(
      values = c(col_palette),
      name = "Warming"
    ) +
  
    scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +
  
    scale_fill_manual(values = c("grey30", "white", "#FD6467", "white"),
    name = "Origin",
    labels = c("Alpine", "Sub-Alpine", "", ""),
    guide = guide_legend(override.aes = list(
      shape = 21,
      linetype = c("solid", "dashed"),
      colour = "grey30",
      fill = c("grey30", "white")
    ))
  ) +
  
    scale_linetype_manual(
      values = c("solid", "dashed"),
      name = "Clipping"
    ) +
  
  scale_alpha_manual(values = c(1, 0.6)) +   
      
  labs(y = "Trait mean",
       x = "Log(Nitrogen)") +
  
  # Faceting
  facet_grid(
    rows = vars(figure_names), 
    cols = vars(status2),
    labeller = label_parsed, 
    scales = "free"
  ) +
  
  theme_bw()


}
