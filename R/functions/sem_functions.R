# Structural equation model functions

# prep data for SEM
prep_SEM_data <- function(data, landuse, diversity, biomass){

  data <- data |>
    rename(.diversity = {{diversity}},
           .biomass = {{biomass}}) |>
    mutate(warming = if_else(warming == "Ambient", 0, 1),
           nitrogen = Nitrogen_log,
           # Alpine is 0 and Sub-alpine is 1 (Alpine is first)
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

  } else if (!landuse %in% c("clipping", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# run SEM
run_SEM <- function(data, landuse, change){

  if(landuse == "clipping"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + clipping, data),
      lm(biomass ~ warming + nitrogen + clipping, data)
    )

  } else if (landuse == "grazing"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + grazing, data),
      lm(biomass ~ warming + nitrogen + grazing, data)
    )

  } else if (!landuse %in% c("clipping", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# make SEM figure
make_SEM_figure <- function(sem_results, type, landuse, col, diversity_type = "diversity"){

  # path and estimates
  paths <- tibble(from = sem_results$coefficients$Predictor,
                  to = sem_results$coefficients$Response,
                  label = round(sem_results$coefficients$Std.Estimate, 3),
                  P.Value = sem_results$coefficients$P.Value) |>
    mutate(
           # Line type: solid (1) for positive, dashed (2) for negative
           linetype = if_else(label >= 0, 1, 2),
           # Color: treatment colors for significant, grey for non-significant
           colour = case_when(
             P.Value > 0.05 ~ "grey60",
             from == "warming" ~ col[2],      # color 2 (red)
             from == "nitrogen" ~ col[4],      # color 4 (green)
             from == "clipping" ~ col[3],      # color 3 (yellow)
             from == "grazing" ~ col[3],       # color 3 (yellow)
             from == "biomass" ~ col[1],       # color 1 (grey)
             TRUE ~ col[1]                    # default color
           ),
           # Size: scale with absolute value of standardized estimate
           size = abs(label) * 3,             # multiply by 3 to make differences more visible
           # Replace "diversity" with the actual diversity type for display
           to = case_when(to == "diversity" ~ diversity_type,
                          TRUE ~ to))

  if(type == "final"){

    paths <- paths

    if(landuse == "clipping"){

      layout = matrix(c('warming', '', '', '',
                        '', 'biomass', '', diversity_type,
                        'nitrogen','', '', '',
                        '', 'clipping', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('warming', '', '', '',
                        '', 'biomass', '', diversity_type,
                        'nitrogen','', '', '',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("clipping", "grazing")){
      print("Warning, unknown landuse variable")
    }

  } else if (type == "change"){

    paths <- paths |>
      mutate(from = case_when(from == "biomass" ~ "Δbiomass",
                              .default = from),
             to = case_when(to == "biomass" ~ "Δbiomass",
                            to == "diversity" ~ paste0("Δ", diversity_type),
                            TRUE ~ from))
    if(landuse == "clipping"){

      layout = matrix(c('warming', '', '', '',
                        '', 'Δbiomass', '', paste0('Δ', diversity_type),
                        'nitrogen','', '', '',
                        '', 'clipping', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('warming', '', '', '',
                        '', 'Δbiomass', '', paste0('Δ', diversity_type),
                        'nitrogen','', '', '',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("clipping", "grazing")){
      print("Warning, unknown landuse variable")
    }

  }

  # Create nodes with colors - dynamically add the diversity type
  all_nodes <- c("warming", "nitrogen", "biomass", diversity_type, "clipping", "grazing", "Δbiomass", paste0("Δ", diversity_type))
  
  nodes <- tibble(
    name = all_nodes,
    label_color = case_when(
      name %in% c("biomass", diversity_type, "Δbiomass", paste0("Δ", diversity_type)) ~ col[1],  # color 1 (grey)
      name == "warming" ~ col[2],                                              # color 2 (red)
      name %in% c("clipping", "grazing") ~ col[3],                            # color 3 (yellow)
      name == "nitrogen" ~ col[4],                                            # color 4 (green)
      TRUE ~ "black"                                                          # default
    )
  )
  
  # Plot SEM with tidySEM
  plot_model <- prepare_graph(edges = paths, nodes = nodes, layout = layout)
  plot(plot_model)

}
