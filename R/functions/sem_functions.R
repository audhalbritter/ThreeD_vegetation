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

  if(landuse == "cutting"){
    data |>
      filter(grazing != "Natural") |>
      mutate(cutting = grazing_num)

  } else if (landuse == "grazing"){

    data |>
      filter(grazing %in% c("Natural", "Control")) |>
      mutate(grazing = if_else(grazing == "Control", 0, 1))

  } else if (!landuse %in% c("cutting", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# run SEM
run_direct_SEM <- function(data, landuse, diversity, biomass){

  if(landuse == "cutting"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + cutting, data)
    )

  } else if (landuse == "grazing"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + grazing, data)
    )

  } else if (!landuse %in% c("cutting", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# run SEM
run_site_SEM <- function(data, landuse){

    if(landuse == "cutting"){

      model <- psem(
        lm(diversity ~ biomass + warming + nitrogen + cutting + site, data),
        lm(biomass ~ warming + nitrogen + cutting + site, data)
      )

  } else if (landuse == "grazing"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + grazing + site, data),
      lm(biomass ~ warming + nitrogen + grazing + site, data)
    )

  } else if (!landuse %in% c("cutting", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# run SEM
run_SEM <- function(data, landuse, change){

  if(landuse == "cutting"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + cutting, data),
      lm(biomass ~ warming + nitrogen + cutting, data)
    )

  } else if (landuse == "grazing"){

    model <- psem(
      lm(diversity ~ biomass + warming + nitrogen + grazing, data),
      lm(biomass ~ warming + nitrogen + grazing, data)
    )

  } else if (!landuse %in% c("cutting", "grazing")){
    print("Warning, unknonw landuse variable")
  }

}


# make SEM figure
make_SEM_figure <- function(sem_results, type, landuse, col){

  # path and estimates
  paths <- tibble(from = sem_results$coefficients$Predictor,
                  to = sem_results$coefficients$Response,
                  label = round(sem_results$coefficients$Std.Estimate, 3),
                  P.Value = sem_results$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5))

  if(type == "final"){

    paths <- paths

    if(landuse == "cutting"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'biomass', '','diversity',
                        'warming','', '', '',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'biomass', '','diversity',
                        'warming','', '', '',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("cutting", "grazing")){
      print("Warning, unknown landuse variable")
    }

  } else if (type == "change"){

    paths <- paths |>
      mutate(from = case_when(from == "biomass" ~ "Δbiomass",
                              .default = from),
             to = case_when(to == "biomass" ~ "Δbiomass",
                            to == "diversity" ~ "Δdiversity",
                            TRUE ~ from))
    if(landuse == "cutting"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '','Δdiversity',
                        'warming','', '', '',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '','Δdiversity',
                        'warming','', '', '',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("cutting", "grazing")){
      print("Warning, unknown landuse variable")
    }

  }

  # Plot SEM with tidySEM
  plot_model <- prepare_graph(edges = paths, layout = layout)
  plot(plot_model)

}


make_SEM_site_figure <- function(sem_results, type, landuse, col){

  # path and estimates
  paths <- tibble(from = sem_results$coefficients$Predictor,
         to = sem_results$coefficients$Response,
         label = round(sem_results$coefficients$Std.Estimate, 3),
         P.Value = sem_results$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5))

  if(type == "final"){

    paths <- paths

    if(landuse == "cutting"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'biomass', '','diversity',
                        'warming','', '', 'site',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'biomass', '', 'diversity',
                        'warming','', '', 'site',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("cutting", "grazing")){
      print("Warning, unknown landuse variable")
    }

  } else if (type == "change"){

    paths <- paths |>
      mutate(from = case_when(from == "biomass" ~ "Δbiomass",
                              .default = from),
             to = case_when(to == "biomass" ~ "Δbiomass",
                            to == "diversity" ~ "Δdiversity",
                            TRUE ~ from))
    if(landuse == "cutting"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '','Δdiversity',
                        'warming','', '', 'site',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (landuse == "grazing"){

      layout = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '', 'Δdiversity',
                        'warming','', '', 'site',
                        '', 'grazing', '', ''),
                      nrow = 4, byrow = TRUE)

    } else if (!landuse %in% c("cutting", "grazing")){
      print("Warning, unknown landuse variable")
    }

  }

  # Plot SEM with tidySEM
  plot_model <- prepare_graph(edges = paths, layout = layout)
  plot(plot_model)

}
