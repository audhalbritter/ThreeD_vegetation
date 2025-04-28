
make_sem_div <- function(cut_change_data, sem_colour){

  col <- sem_colour

  dd <- cut_change_data |>
    rename(W = warming,
           N = nitrogen,
           Cut = cutting)

  layout1 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δdiversity',
                     'N:Cut', '', '','', '',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  layout2 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δdiversity',
                     'N:Cut', '', '','', 'site',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  # across sites
  model1 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd),
    lm(biomass ~ W * N * Cut, dd)
  )

  sem_results1 <- summary(model1)

  # path and estimates
  paths1 <- tibble(from = sem_results1$coefficients$Predictor,
                   to = sem_results1$coefficients$Response,
                   label = round(sem_results1$coefficients$Std.Estimate, 3),
                   P.Value = sem_results1$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δdiversity",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
  p1 <- plot(plot_model1)

  # with sites
  model2 <- psem(
    lm(diversity ~ biomass + W * N * Cut + site, dd),
    lm(biomass ~ W * N * Cut + site, dd)
  )

  sem_results2 <- summary(model2)

  # path and estimates
  paths2 <- tibble(from = sem_results2$coefficients$Predictor,
                   to = sem_results2$coefficients$Response,
                   label = round(sem_results2$coefficients$Std.Estimate, 3),
                   P.Value = sem_results2$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δdiversity",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
  p2 <- plot(plot_model2)


  # alpine
  model3 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Alpine"))
  )

  sem_results3 <- summary(model3)

  # path and estimates
  paths3 <- tibble(from = sem_results3$coefficients$Predictor,
                   to = sem_results3$coefficients$Response,
                   label = round(sem_results3$coefficients$Std.Estimate, 3),
                   P.Value = sem_results3$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δdiversity",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
  p3 <- plot(plot_model3)


  # sub-alpine
  model4 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine"))
  )

  sem_results4 <- summary(model4)

  # path and estimates
  paths4 <- tibble(from = sem_results4$coefficients$Predictor,
                   to = sem_results4$coefficients$Response,
                   label = round(sem_results4$coefficients$Std.Estimate, 3),
                   P.Value = sem_results4$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δdiversity",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
  p4 <- plot(plot_model4)


  (p1 + p2) /
    (p3 + p4) +
    plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 12, hjust = 0, vjust = 0))


}


make_sem_rich <- function(biomass_div, sem_colour){

  col <- sem_colour

  dd <- biomass_div |>
    rename(diversity = log_ratio_richness,
           biomass = log_ratio_bio) |>
    mutate(warming = if_else(warming == "Ambient", 0, 1),
           nitrogen = Nitrogen_log,
           # Alpine is 0 and Sub-alpine is 1 (Alpine is first)
           site = if_else(origSiteID == "Alpine", 0, 1)) |>
    filter(grazing != "Natural") |>
    mutate(cutting = grazing_num) |>
    rename(W = warming,
           N = nitrogen,
           Cut = cutting)

  layout1 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δrichness',
                     'N:Cut', '', '','', '',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  layout2 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δrichness',
                     'N:Cut', '', '','', 'site',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  # across sites
  model1 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd),
    lm(biomass ~ W * N * Cut, dd)
  )

  sem_results1 <- summary(model1)

  # path and estimates
  paths1 <- tibble(from = sem_results1$coefficients$Predictor,
                   to = sem_results1$coefficients$Response,
                   label = round(sem_results1$coefficients$Std.Estimate, 3),
                   P.Value = sem_results1$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δrichness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
  p1 <- plot(plot_model1)

  # with sites
  model2 <- psem(
    lm(diversity ~ biomass + W * N * Cut + site, dd),
    lm(biomass ~ W * N * Cut + site, dd)
  )

  sem_results2 <- summary(model2)

  # path and estimates
  paths2 <- tibble(from = sem_results2$coefficients$Predictor,
                   to = sem_results2$coefficients$Response,
                   label = round(sem_results2$coefficients$Std.Estimate, 3),
                   P.Value = sem_results2$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δrichness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
  p2 <- plot(plot_model2)


  # alpine
  model3 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Alpine"))
  )

  sem_results3 <- summary(model3)

  # path and estimates
  paths3 <- tibble(from = sem_results3$coefficients$Predictor,
                   to = sem_results3$coefficients$Response,
                   label = round(sem_results3$coefficients$Std.Estimate, 3),
                   P.Value = sem_results3$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δrichness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
  p3 <- plot(plot_model3)


  # sub-alpine
  model4 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine"))
  )

  sem_results4 <- summary(model4)

  # path and estimates
  paths4 <- tibble(from = sem_results4$coefficients$Predictor,
                   to = sem_results4$coefficients$Response,
                   label = round(sem_results4$coefficients$Std.Estimate, 3),
                   P.Value = sem_results4$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δrichness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
  p4 <- plot(plot_model4)


  (p1 + p2) /
    (p3 + p4) +
    plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 12, hjust = 0, vjust = 0))


}




make_sem_even <- function(biomass_div, sem_colour){

  col <- sem_colour

  dd <- biomass_div |>
    rename(diversity = log_ratio_evenness,
           biomass = log_ratio_bio) |>
    mutate(warming = if_else(warming == "Ambient", 0, 1),
           nitrogen = Nitrogen_log,
           # Alpine is 0 and Sub-alpine is 1 (Alpine is first)
           site = if_else(origSiteID == "Alpine", 0, 1)) |>
    filter(grazing != "Natural") |>
    mutate(cutting = grazing_num) |>
    rename(W = warming,
           N = nitrogen,
           Cut = cutting)

  layout1 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δevenness',
                     'N:Cut', '', '','', '',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  layout2 = matrix(c('', 'W:N', 'W', '', '',
                     'N', '', '','', '',
                     '', '', 'Δbiomass','', 'Δevenness',
                     'N:Cut', '', '','', 'site',
                     '','Cut', '', 'W:Cut', '',
                     '', '', 'W:N:Cut', '', ''),
                   nrow = 6, byrow = TRUE)

  # across sites
  model1 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd),
    lm(biomass ~ W * N * Cut, dd)
  )

  sem_results1 <- summary(model1)

  # path and estimates
  paths1 <- tibble(from = sem_results1$coefficients$Predictor,
                   to = sem_results1$coefficients$Response,
                   label = round(sem_results1$coefficients$Std.Estimate, 3),
                   P.Value = sem_results1$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δevenness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
  p1 <- plot(plot_model1)

  # with sites
  model2 <- psem(
    lm(diversity ~ biomass + W * N * Cut + site, dd),
    lm(biomass ~ W * N * Cut + site, dd)
  )

  sem_results2 <- summary(model2)

  # path and estimates
  paths2 <- tibble(from = sem_results2$coefficients$Predictor,
                   to = sem_results2$coefficients$Response,
                   label = round(sem_results2$coefficients$Std.Estimate, 3),
                   P.Value = sem_results2$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δevenness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
  p2 <- plot(plot_model2)


  # alpine
  model3 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Alpine"))
  )

  sem_results3 <- summary(model3)

  # path and estimates
  paths3 <- tibble(from = sem_results3$coefficients$Predictor,
                   to = sem_results3$coefficients$Response,
                   label = round(sem_results3$coefficients$Std.Estimate, 3),
                   P.Value = sem_results3$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δevenness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
  p3 <- plot(plot_model3)


  # sub-alpine
  model4 <- psem(
    lm(diversity ~ biomass + W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine")),
    lm(biomass ~ W * N * Cut, dd |>
         filter(origSiteID == "Sub-alpine"))
  )

  sem_results4 <- summary(model4)

  # path and estimates
  paths4 <- tibble(from = sem_results4$coefficients$Predictor,
                   to = sem_results4$coefficients$Response,
                   label = round(sem_results4$coefficients$Std.Estimate, 3),
                   P.Value = sem_results4$coefficients$P.Value) |>
    mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
           colour = if_else(label > 0, col[1], col[2]),
           size = case_when(P.Value <= 0.05 ~ 1.5,
                            P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                            TRUE ~ 0.5),
           from = case_when(from == "biomass" ~ "Δbiomass",
                            .default = from),
           to = case_when(to == "biomass" ~ "Δbiomass",
                          to == "diversity" ~ "Δevenness",
                          TRUE ~ from))

  # Plot SEM with tidySEM
  plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
  p4 <- plot(plot_model4)


  (p1 + p2) /
    (p3 + p4) +
    plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 12, hjust = 0, vjust = 0))


}
