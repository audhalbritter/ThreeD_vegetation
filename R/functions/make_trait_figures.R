# Make trait figures  


# Ridgeline plot function for trait distributions
make_trait_ridgeline_plot <- function(data, group_var, custom_colors = NULL, n_bins = 5, y_axis_label = NULL, figure_names_order = NULL, legend_name = NULL) {
  
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
  
  # Apply custom figure_names order if provided
  if (!is.null(figure_names_order) && "figure_names" %in% names(plot_data)) {
    plot_data <- plot_data |>
      mutate(figure_names = factor(figure_names, levels = figure_names_order))
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
      scale_fill_manual(values = custom_colors, name = legend_name) +
      scale_color_manual(values = custom_colors, name = legend_name)
  }
  
  return(plot)
}

# Add significance stars to trait distribution plots
add_significance_stars <- function(plot, trait_stats, treatment_type) {
  
  # Extract significance data for the specific treatment
  sig_data <- trait_stats |>
    unnest(anova_tidy) |>
    filter(treatment == treatment_type) |>
    # Get the main effect term (not intercept)
    filter(term != "(Intercept)") |>
    # Create significance labels
    mutate(
      significance = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**", 
        p.value <= 0.05 ~ "*",
        TRUE ~ ""
      )
    ) |>
    select(trait_trans, origSiteID, significance)
  
  # Create annotation data frame that matches the facet structure
  # We need to match trait_trans to figure_names format
  annotation_data <- sig_data |>
    # Convert trait_trans to figure_names format
    mutate(
      figure_names = case_when(
        trait_trans == "temperature" ~ "Temperature",
        trait_trans == "light" ~ "Light",
        trait_trans == "moisture" ~ "Moisture",
        trait_trans == "nutrients" ~ "Nutrients",
        trait_trans == "reaction" ~ "Reaction",
        trait_trans == "grazing_pressure" ~ "Grazing",
        TRUE ~ trait_trans
      ),
      # Add positioning variables
      x = Inf,
      y = Inf
    )
  
  # Extract factor levels from the original plot to preserve facet ordering
  # Get the data from the plot's layers to find the factor levels
  plot_data <- ggplot_build(plot)$data[[1]]  # Get data from the first layer
  if ("PANEL" %in% names(plot_data)) {
    # Extract the facet levels from the plot
    facet_levels <- levels(plot_data$PANEL)
    # Convert panel names back to figure_names (this assumes the facet structure)
    # We need to get the actual figure_names levels from the original data
    original_data <- plot$data
    if ("figure_names" %in% names(original_data)) {
      figure_names_levels <- levels(original_data$figure_names)
      if (!is.null(figure_names_levels)) {
        annotation_data <- annotation_data |>
          mutate(figure_names = factor(figure_names, levels = figure_names_levels))
      }
    }
  }
  
  # Add significance stars to the plot
  plot_with_stars <- plot +
    geom_text(
      data = annotation_data,
      aes(x = x, y = y, label = significance),
      hjust = 1.2,
      vjust = 1.2,
      size = 6,
      fontface = "bold",
      inherit.aes = FALSE
    )
  
  return(plot_with_stars)
}

