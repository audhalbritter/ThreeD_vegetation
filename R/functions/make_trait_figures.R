# Make trait figures  

### TRAIT PCA ###

# Make pca
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # make wide trait table
  cwm_wide <- trait_mean |>
    select(trait_trans, turfID, blockID, origSiteID, warming, grazing, grazing_num, Nlevel,Namount_kg_ha_y, Nitrogen_log, mean) |>
    pivot_wider(names_from = "trait_trans", values_from = "mean") |>
    ungroup()

  pca_output <- cwm_wide |>
    select(-(turfID:Nitrogen_log)) |>
    rda(scale = TRUE, center = TRUE)

  pca_sites <- bind_cols(
    cwm_wide |>
      select(turfID:Nitrogen_log),
    fortify(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify(pca_output, display = "species") |>
    mutate(trait_trans = label) |>
    fancy_trait_name_dictionary()

  # permutation test
  raw <- cwm_wide |> select(-(turfID:Nitrogen_log))
  # meta data
  meta <- cwm_wide|> select(turfID:Nitrogen_log) |>
    mutate(origSiteID = factor(origSiteID))

  # adonis test
  #adonis_result <- adonis2(raw ~ warming * Nitrogen_log * grazing_num + origSiteID, data = meta, permutations = 999, method = "euclidean", by = "terms")

  outputList <- list(pca_sites, pca_traits, pca_output)

}

# Make pca plot
make_pca_plot <- function(trait_pca, title = NULL, color_warm = NULL){

  e_B1 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  trait_pca[[1]] |>
    ggplot(aes(x = PC1, y = PC2, colour = warming, fill = warming, shape = grazing, size = Nitrogen_log)) +
    geom_point(alpha = 0.5) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE, colour = "grey60") +
    geom_text(data = trait_pca[[2]] |>
                mutate(figure_names = case_match(figure_names,
                                                 "Plant~height~(cm)" ~ "Height~(cm)",
                                                 "Leaf~dry~mass~(g)" ~ "Dry~mass~(g)",
                                                 "Leaf~area~(cm^2)" ~ "Area~(cm^2)",
                                                 "Leaf~thickness~(mm)" ~ "Thickness~(mm)",
                                                 .default = figure_names)),
                      #  PC2 = case_when(label == "leaf_area_cm2_log" ~ -0.2,
                      #                  label == "leaf~dry~mass~(g)" ~ -0.1,
                      #                  label == "sla_cm2_g" ~ -1.1,
                      #                  TRUE ~ PC2),
                      #  PC1 = case_when(label == "leaf_thickness_mm_log" ~ 0.7,
                      #                  TRUE ~ PC1)),
              aes(x = PC1 + 0.3, y = PC2 + 0.2, label = figure_names),
              size = 3,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    scale_fill_manual(name = "Warming", values = color_warm) +
    scale_colour_manual(name = "Warming", values = color_warm) +
    scale_shape_manual(name = "Clipping", values = c(21, 22, 24)) +
    labs(title = title,
         x = glue("PCA1 ({round(e_B1[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B1[2] * 100, 1)}%)")) +
    theme_bw()

}

# Make pca plot
make_pca_plot_sites <- function(trait_pca, title = NULL, color_warm = NULL){

  e_B1 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  # Create site_graz interaction and ensure correct order
  plot_data <- trait_pca[[1]] |>
    mutate(site_graz = paste(origSiteID, grazing, sep = "."),
           site_graz = factor(site_graz, 
                            levels = c("Alpine.Control", "Alpine.Medium", "Alpine.Intensive",
                                     "Sub-alpine.Control", "Sub-alpine.Medium", "Sub-alpine.Intensive")))

  # Create the plot
  # Split data for filled and unfilled points
  alpine_data <- plot_data |> filter(!str_detect(site_graz, "Sub-alpine"))
  subalpine_data <- plot_data |> filter(str_detect(site_graz, "Sub-alpine"))
  
  # Create base plot
  ggplot() +
    # Add filled points for Alpine
    geom_point(data = alpine_data,
               aes(x = PC1, y = PC2, colour = warming, fill = warming, 
                   shape = site_graz, size = Nitrogen_log),
               alpha = 0.5) +
    # Add unfilled points for Sub-alpine
    geom_point(data = subalpine_data,
               aes(x = PC1, y = PC2, colour = warming, 
                   shape = site_graz, size = Nitrogen_log),
               fill = "white",
               alpha = 0.5) +
    # Add arrows for trait loadings
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE, colour = "grey60") +
    # Add trait labels
    geom_text(data = trait_pca[[2]] |>
                mutate(figure_names = case_match(figure_names,
                                                 "Plant~height~(cm)" ~ "Height~(cm)",
                                                 "Leaf~dry~mass~(g)" ~ "Dry~mass~(g)",
                                                 "Leaf~area~(cm^2)" ~ "Area~(cm^2)",
                                                 "Leaf~thickness~(mm)" ~ "Thickness~(mm)",
                                                 .default = figure_names)),
              aes(x = PC1 + 0.3, y = PC2 + 0.2, label = figure_names),
              size = 3,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    # Scales and theme
    coord_equal() +
    scale_fill_manual(name = "Warming", values = color_warm) +
    scale_colour_manual(name = "Warming", values = color_warm) +
    scale_shape_manual(name = "Site & Clipping", 
                      values = c("Alpine.Control" = 16,
                                "Alpine.Medium" = 15, 
                                "Alpine.Intensive" = 17,
                                "Sub-alpine.Control" = 21,
                                "Sub-alpine.Medium" = 22, 
                                "Sub-alpine.Intensive" = 24)) +
    labs(title = title,
         x = glue("PCA1 ({round(e_B1[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B1[2] * 100, 1)}%)")) +
    theme_bw() +
    guides(size = guide_legend(title = "Nitrogen (kg/ha/y)"))

}


# Ridgeline plot function for trait distributions
make_trait_ridgeline_plot <- function(data, group_var, custom_colors = NULL, n_bins = 5, y_axis_label = NULL, figure_names_order = NULL) {
  
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
      scale_fill_manual(values = custom_colors) +
      scale_color_manual(values = custom_colors)
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
        trait_trans == "plant_height_cm_log" ~ "Plant~height~(cm)",
        trait_trans == "temperature" ~ "Temperature",
        trait_trans == "light" ~ "Light",
        trait_trans == "moisture" ~ "Moisture",
        trait_trans == "nutrients" ~ "Nutrients",
        trait_trans == "reaction" ~ "Reaction",
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

