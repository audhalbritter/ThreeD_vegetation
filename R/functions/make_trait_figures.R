# Make trait figures

make_trait_figure <- function(traits, traits_stats, trait_prediction, col_palette){

  max_y <- traits |>
  group_by(figure_names) |>
  summarise(y_max = max(mean, na.rm = TRUE), .groups = "drop")


traits_text <- traits_stats |>
  filter(p.value <= 0.05) |>
  mutate(nr = 1:n(), .by = c(origSiteID, status, trait_trans, figure_names)) |>
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
             cols = vars(status),
             scales = "free_y", labeller = label_parsed) +
  theme_bw()
  
}

make_trait_figure_small <- function(traits, traits_stats, trait_prediction_clean, col_palette){

  warm <- traits_stats |>
  filter(p.value <= 0.05) |> 
  filter(trait_trans %in% c("plant_height_cm_log", "temperature"),
          term == "W")

fig_warm <- traits |> 
  tidylog::inner_join(warm, by = c("origSiteID", "status", "trait_trans", "figure_names")) |> 
  ggplot(aes(x = status, y = mean, fill = warming)) +
  geom_violin(draw_quantiles = 0.5, alpha = 0.8) +
  scale_fill_manual(values = col_palette, name = "Warming") +
  labs(tag = "a)") +
  facet_grid(rows = vars(figure_names), cols = vars(origSiteID),
              labeller = label_parsed) +
  theme_bw()


nitrogen <- traits_stats |>
  filter(p.value <= 0.05) |> 
  filter(trait_trans %in% c("plant_height_cm_log", "temperature", "light"),
          term %in% c("N", "WxN"))
  
prediction <- trait_prediction_clean |> 
  tidylog::inner_join(nitrogen, by = c("origSiteID", "status", "trait_trans", "figure_names"))

pred_N <- prediction |> 
  filter(term == "N") |> 
  # remove if N and WxN
  filter(!c(origSiteID == "Alpine" & status == "extinction" & trait_trans == "light")) |> 
  filter(!c(origSiteID == "Alpine" & status == "increase" & trait_trans == "light")) |> 
  filter(!c(origSiteID == "Sub-alpine" & status == "increase" & trait_trans == "plant_height_cm_log")) |> 
  group_by(origSiteID, status, trait_trans, figure_names, Nitrogen_log) |> 
  summarise(fit = mean(fit))

pred_WxN <- prediction |> 
  filter(term == "WxN") |> 
  group_by(origSiteID, status, trait_trans, figure_names, Nitrogen_log, warming) |> 
  summarise(fit = mean(fit))


fig_nitro <- traits |> 
  tidylog::inner_join(nitrogen, by = c("origSiteID", "status", "trait_trans", "figure_names")) |> 
  ggplot(aes(
    x = Nitrogen_log, 
    y = mean,
    linetype = status  # Global linetype mapping
  )) +
  
  # Points: Warming colors (only for NxW, but also visible for N as grey)
  geom_point(aes(color = warming)) +
  
  geom_line(data = pred_N,
    aes(y = fit),
    colour = "grey30",
    linewidth = 0.5) +

  geom_line(data = pred_WxN,
    aes(y = fit, colour = warming),
        linewidth = 0.5) +
  
    # Define color mapping
    scale_colour_manual(
      values = c(col_palette),
      name = "Warming"
    ) +
  
    scale_linetype_manual(
      values = c("solid", "dotted", "dashed"),
      name = ""
    ) +
  
    labs(tag = "b)") +
  
  # Faceting
  facet_grid(
    rows = vars(figure_names), 
    cols = vars(origSiteID),
    labeller = label_parsed, 
    scales = "free"
  ) +
  
  theme_bw()

fig_warm / fig_nitro + plot_layout(guides = "collect",
                                   heights = c(2, 3)) &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

}
