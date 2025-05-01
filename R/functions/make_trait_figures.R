# Make trait figures

make_trait_figure <- function(traits, traits_stats, pred, col_palette){

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
  
}