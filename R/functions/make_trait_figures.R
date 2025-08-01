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


make_trait_figure_grazing <- function(traits, traits_stats, trait_prediction_clean, col_palette){

  # average for winners and losers
  traits_wl <- traits |> 
    filter(trait_trans %in% c("plant_height_cm_log", "temperature")) |>
    filter(status2 == "loser", origSiteID == "Sub-alpine") |> 
    group_by(origSiteID, status2, trait_trans, figure_names, Nitrogen_log, warming, grazing) |> 
    summarise(mean = mean(mean))

  prediction_h <- trait_prediction_clean |> 
    filter(trait_trans %in% c("plant_height_cm_log")) |>
    filter(status2 == "loser", origSiteID == "Sub-alpine") |> 
    group_by(origSiteID, status2, trait_trans, figure_names, warming, grazing, Nitrogen_log) |> 
    summarise(fit = mean(fit))

  prediction_t <- trait_prediction_clean |> 
    filter(trait_trans %in% c("temperature")) |>
    filter(status2 == "loser", origSiteID == "Sub-alpine") |> 
    group_by(origSiteID, status2, trait_trans, figure_names, warming, grazing, Nitrogen_log) |> 
    summarise(fit = mean(fit))
    
  # figure text
  max_y <- traits_wl |>
    group_by(figure_names) |>
    summarise(y_max = max(mean, na.rm = TRUE), .groups = "drop")
  
  
  traits_text <- traits_stats |>
    filter(p.value <= 0.05) |>
    filter(trait_trans %in% c("plant_height_cm_log", "temperature")) |> 
    filter(status2 == "loser", origSiteID == "Sub-alpine") |> 
    mutate(nr = 1:n(), .by = c(origSiteID, status2, trait_trans, figure_names)) |>
    left_join(max_y, by = c("figure_names")) |>
    mutate(x = if_else(origSiteID == "Alpine", -Inf, Inf),
           y = y_max + nr * 0.05 * y_max,
           hjust = if_else(origSiteID == "Alpine", 0, 1))
  
# Plant height
height <- traits_wl |> 
  filter(trait_trans == "plant_height_cm_log") |> 
  ggplot(aes(
    x = Nitrogen_log, 
    y = mean,
    linetype = grazing
  )) +
  
  # Points: Warming colors (only for NxW, but also visible for N as grey)
  geom_point(aes(shape = grazing,
                 color = warming,)) +
  
  geom_line(data = prediction_h,
    aes(y = fit, alpha = origSiteID),
    linewidth = 0.5
  ) +
  
  #geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
  
  geom_text(
    data = traits_text |> 
      filter(trait_trans == "plant_height_cm_log"),
    aes(x = x, y = y, label = term,
        hjust = hjust),
        alpha = 0.6,
        inherit.aes = FALSE,
        size = 3
      ) +
  
  # Define color mapping
  scale_colour_manual(
      values = c(col_palette),
      name = "Warming"
    ) +
  
  scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +
  
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  
  scale_alpha_manual(values = c(1, 0.6)) +   
      
  labs(y = "Trait mean",
       x = "Log(Nitrogen)") +
  
  facet_wrap(vars(figure_names),
            labeller = label_parsed) +
  
  theme_bw()



  # Temperature
 temp <- traits_wl |> 
  filter(trait_trans == "temperature") |> 
  ggplot(aes(
    x = Nitrogen_log, 
    y = mean,
    color = warming,
    linetype = grazing
  )) +
  
  # Points: Warming colors (only for NxW, but also visible for N as grey)
  geom_point(aes(shape = grazing)) +
  
  geom_line(data = prediction_t,
    aes(y = fit, alpha = origSiteID),
    linewidth = 0.5
  ) +
  
  geom_text(
    data = traits_text |> 
      filter(trait_trans == "temperature",
             term == "WxC"),
    aes(x = x, y = y, label = term,
        hjust = hjust),
        alpha = 0.6,
        inherit.aes = FALSE,
        size = 3
      ) +
  
  # Define color mapping
  scale_colour_manual(
      values = c(col_palette),
      name = "Warming"
    ) +
  
  scale_shape_manual(values = c(21, 22, 24, 23), name = "Grazing") +

  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  
  scale_alpha_manual(values = c(1, 0.6)) +   
      
  labs(y = "Trait mean",
       x = "Log(Nitrogen)") +
  
  facet_wrap(vars(figure_names),
            labeller = label_parsed, 
            scales = "free", nrow = 2) +
  
  theme_bw()

  height / temp + plot_layout(guides = "collect")
}



# Ridgeline plot function for trait distributions
make_trait_ridgeline_plot <- function(data, group_var, custom_colors = NULL, n_bins = 5, y_axis_label = NULL) {
  
  # Check if ggridges package is available
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Package 'ggridges' is required for ridgeline plots. Please install it with: install.packages('ggridges')")
  }
  
  # Validate input
  if (!group_var %in% names(data)) {
    stop(paste("Variable", group_var, "not found in data"))
  }
  
  # Check if grouping variable is numeric
  is_numeric <- is.numeric(data[[group_var]])
  
  # Prepare data based on variable type
  if (is_numeric) {
    # For numeric variables, create bins
    plot_data <- data |>
      mutate(
        !!paste0(group_var, "_binned") := cut(!!sym(group_var), 
                                              breaks = n_bins, 
                                              include.lowest = TRUE,
                                              dig.lab = 3)
      ) |>
      rename(group_binned = !!paste0(group_var, "_binned"))
    
    # Use binned variable for plotting
    y_var <- "group_binned"
    fill_var <- "group_binned"
    
  } else {
    # For factor variables, use as is
    plot_data <- data
    y_var <- group_var
    fill_var <- group_var
  }
  
  # Create the ridgeline plot
  plot <- plot_data |>
    ggplot(aes(x = mean, 
               y = !!sym(y_var), 
               fill = !!sym(fill_var),
               color = !!sym(fill_var))) +
    ggridges::geom_density_ridges(
      alpha = 0.5,
      scale = 2,
      rel_min_height = 0.01,
      quantile_lines = TRUE,
      quantiles = 2
    ) +
    labs(
      x = "Trait mean",
      y = ifelse(!is.null(y_axis_label), y_axis_label, 
                 ifelse(is_numeric, paste(group_var, "(binned)"), group_var))
    ) +
    facet_grid(origSiteID ~ figure_names, 
               scales = "free_x", 
               labeller = labeller(figure_names = label_parsed)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Add custom colors if provided
  if (!is.null(custom_colors)) {
    plot <- plot + 
      scale_fill_manual(values = custom_colors) +
      scale_color_manual(values = custom_colors)
  }
  
  return(plot)
}

