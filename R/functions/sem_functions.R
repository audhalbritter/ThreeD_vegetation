# Structural equation model helpers: data prep, psem fitting, and ggraph path figures



# prep data for SEM
prep_SEM_data <- function(data, landuse, diversity, biomass){

  data <- data |>
    rename(.diversity = {{diversity}},
           .biomass = {{biomass}}) |>
    mutate(warming = if_else(warming == "Ambient", 0, 1),
           nitrogen = Nitrogen_log,
           site = if_else(origSiteID == "Alpine", 0, 1)) |>
    rename(diversity = .diversity,
           biomass = .biomass)

  if(landuse == "clipping"){
    data |>
      filter(grazing != "Natural") |>
      mutate(clipping = grazing_num)

  } else if (landuse == "grazing"){

    data |>
      filter(grazing %in% c("Natural", "Control")) |>
      mutate(grazing = if_else(grazing == "Control", 0, 1))

  } else {
    warning("unknown landuse variable", call. = FALSE)
    data
  }

}


# run SEM — returns fitted psem object
run_SEM <- function(data, landuse){

  if (landuse == "clipping") {
    return(psem(
      lm(diversity ~ biomass + warming + nitrogen + clipping, data),
      lm(biomass ~ warming + nitrogen + clipping, data)
    ))
  }

  if (landuse == "grazing") {
    return(psem(
      lm(diversity ~ biomass + warming + nitrogen + grazing, data),
      lm(biomass ~ warming + nitrogen + grazing, data)
    ))
  }

  warning("Unknown landuse variable.", call. = FALSE)
  invisible(NULL)
}


.sem_piecewise_SEM_layout_matrix <- function(landuse, diversity_type) {
  if (landuse == "clipping") {
    matrix(
      c(
        "warming", "", "", "",
        "", "biomass", "", diversity_type,
        "nitrogen", "", "", "",
        "", "clipping", "", ""
      ),
      nrow = 4,
      byrow = TRUE
    )
  } else if (landuse == "grazing") {
    matrix(
      c(
        "warming", "", "", "",
        "", "biomass", "", diversity_type,
        "nitrogen", "", "", "",
        "", "grazing", "", ""
      ),
      nrow = 4,
      byrow = TRUE
    )
  } else {
    stop("Unknown landuse variable.", call. = FALSE)
  }
}


.sem_layout_xy_from_matrix <- function(layout_mat,
                                      spacing_x = 2,
                                      spacing_y = 2) {
  nr <- nrow(layout_mat)
  nc <- ncol(layout_mat)
  out <- list()
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      nm <- trimws(layout_mat[i, j])
      if (nzchar(nm)) {
        out[[length(out) + 1]] <- tibble(
          name = nm,
          x = j * spacing_x,
          y = (nr - i + 1) * spacing_y
        )
      }
    }
  }
  bind_rows(out)
}


.sem_even_clip_graze_distance_from_biomass <- function(node_xy,
                                                       landuse) {
  cg_name <- if (identical(landuse, "clipping")) "clipping" else "grazing"

  bm <- dplyr::filter(node_xy, .data$name == "biomass")
  wm <- dplyr::filter(node_xy, .data$name == "warming")
  cg <- dplyr::filter(node_xy, .data$name == cg_name)

  if (nrow(bm) != 1L || nrow(wm) != 1L || nrow(cg) != 1L) {
    return(node_xy)
  }

  target_d <- sqrt(
    (bm$x - wm$x)^2 + (bm$y - wm$y)^2
  )

  clip_bm_len_factor <- if (identical(landuse, "grazing")) 1.2 else 1.12

  new_y <- bm$y[1L] - target_d * clip_bm_len_factor

  node_xy |>
    dplyr::mutate(
      y = dplyr::if_else(.data$name == cg_name, new_y, .data$y)
    )
}


.sem_nudge_diversity_left <- function(node_xy, diversity_type, delta_x = 1) {
  node_xy |>
    dplyr::mutate(
      x = dplyr::if_else(
        .data$name == .env$diversity_type,
        .data$x - delta_x,
        .data$x
      )
    )
}


.sem_piecewise_SEM_edges_tbl <- function(sem_results,
                                          diversity_type) {
  paths <- tibble(
    from = sem_results$coefficients$Predictor,
    to = sem_results$coefficients$Response,
    estimate = sem_results$coefficients$Std.Estimate,
    P.Value = sem_results$coefficients$P.Value
  ) |>
    filter(from != "(Intercept)") |>
    mutate(
      label = round(.data$estimate, 3),
      edge_colour = dplyr::if_else(
        label > 0,
        colorBlindness::Blue2DarkOrange12Steps[9],
        colorBlindness::Blue2DarkOrange12Steps[1]
      ),
      edge_linetype = dplyr::if_else(P.Value <= 0.05, "solid", "dashed"),
      significance_stars = dplyr::case_when(
        P.Value < 0.001 ~ "***",
        P.Value < 0.01 ~ "**",
        P.Value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      label_txt = paste0(label, significance_stars),
      line_width_base = abs(label) * 4,
      to = dplyr::case_when(
        .data$to == "diversity" ~ diversity_type,
        TRUE ~ .data$to
      )
    )

  paths |>
    dplyr::filter(
      !is.na(.data$from),
      !is.na(.data$to),
      !is.na(.data$estimate),
      !is.infinite(.data$estimate),
      !is.nan(.data$estimate)
    )
}


#' Piecewise SEM path figure (`ggraph`)
#'
#' @param sem_results `summary()` of a `piecewiseSEM::psem()` fit.
#' @param landuse `"clipping"` or `"grazing"`.
#' @param col Passed for API parity / future use (edge colours come from direction).
#' @param diversity_type Diversity node label (e.g. `"diversity"`, `"richness"`, `"evenness"`).
#'
make_SEM_figure <- function(sem_results,
                            landuse,
                            col,
                            diversity_type = "diversity") {
  force(col)

  layout_mat <- .sem_piecewise_SEM_layout_matrix(landuse, diversity_type)

  node_xy <- .sem_layout_xy_from_matrix(layout_mat) |>
    .sem_even_clip_graze_distance_from_biomass(landuse) |>
    .sem_nudge_diversity_left(diversity_type, delta_x = 1)

  edges <- .sem_piecewise_SEM_edges_tbl(
    sem_results,
    diversity_type = diversity_type
  ) |>
    dplyr::semi_join(node_xy, by = c(from = "name")) |>
    dplyr::semi_join(node_xy, by = c(to = "name"))

  if (nrow(edges) == 0L) {
    stop("No edges to plot after matching layout.", call. = FALSE)
  }

  edge_geom <- edges |>
    dplyr::transmute(
      from = .data$from,
      to = .data$to,
      edge_colour = .data$edge_colour,
      edge_linetype = .data$edge_linetype,
      edge_width = pmax(.data$line_width_base, 0.15),
      label_txt = .data$label_txt
    )

  bm_node <- "biomass"

  edge_label_xy <- edge_geom |>
    dplyr::left_join(
      node_xy |> dplyr::rename(x_from = "x", y_from = "y"),
      by = c("from" = "name")
    ) |>
    dplyr::left_join(
      node_xy |> dplyr::rename(x_to = "x", y_to = "y"),
      by = c("to" = "name")
    ) |>
    dplyr::mutate(
      dx = .data$x_to - .data$x_from,
      dy = .data$y_to - .data$y_from,
      len = sqrt(pmax(.data$dx * .data$dx + .data$dy * .data$dy, 1e-6)),
      is_dest_div = .data$to == .env$diversity_type,
      is_land_bm = .data$from %in% c("clipping", "grazing") &
        .data$to == .env$bm_node,
      edge_t = dplyr::case_when(
        .data$from == "nitrogen" & .data$is_dest_div ~ 0.6,
        .data$from == "biomass" & .data$is_dest_div ~ 0.46,
        .data$from == "warming" & .data$is_dest_div ~ 0.54,
        .data$from %in% c("clipping", "grazing") & .data$is_dest_div ~ 0.62,
        .data$is_land_bm ~ 0.34,
        TRUE ~ 0.52
      ),
      perp_mag = 0.07,
      x_on = .data$x_from + .data$edge_t * .data$dx,
      y_on = .data$y_from + .data$edge_t * .data$dy,
      x = .data$x_on + (-.data$dy / .data$len) * .data$perp_mag,
      y = .data$y_on + (.data$dx / .data$len) * .data$perp_mag
    ) |>
    dplyr::select("label_txt", "x", "y")

  g <- tidygraph::tbl_graph(
    nodes = node_xy |> dplyr::select("name"),
    edges = edge_geom
  )

  layout_df <- node_xy |> dplyr::select("x", "y")

  ed_arr <- grid::arrow(
    length = grid::unit(2.85, "mm"),
    type = "closed",
    angle = 18
  )

  ggraph::ggraph(g, layout_df) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_colour = .data$edge_colour,
        edge_linetype = .data$edge_linetype,
        edge_width = .data$edge_width
      ),
      arrow = ed_arr,
      lineend = "butt",
      linejoin = "round",
      alpha = 0.95,
      end_cap = ggraph::circle(11, "mm"),
      start_cap = ggraph::circle(4.5, "mm")
    ) +
    ggplot2::geom_label(
      data = edge_label_xy,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label_txt
      ),
      inherit.aes = FALSE,
      fill = "white",
      colour = "grey10",
      size = 3.2,
      fontface = "plain",
      label.size = 0,
      label.padding = grid::unit(1, "mm"),
      label.r = grid::unit(0.55, "mm")
    ) +
    ggplot2::geom_label(
      data = node_xy,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$name
      ),
      inherit.aes = FALSE,
      fill = "white",
      colour = "black",
      size = 4.6,
      fontface = "plain",
      label.size = 0,
      label.padding = grid::unit(1.4, "mm"),
      label.r = grid::unit(0.55, "mm")
    ) +
    ggraph::scale_edge_width(
      range = c(0.45, 1.35),
      guide = "none"
    ) +
    ggraph::scale_edge_colour_identity() +
    ggraph::scale_edge_linetype_identity() +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 14, 10, 14)
    )
}
