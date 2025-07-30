# si SEM plan

si_SEM_change_plan <- list(

# CHANGE IN BIOMASS AND DIVERSITY (LOG RATIO)
  # CUTTING
  # prep data
  tar_target(
    name = cut_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)
  ),

  # run sem without site
  tar_target(
    name = cut_change,
    command = run_SEM(data = cut_change_data,
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_out,
    command = summary(cut_change)

  ),

  # figure
  tar_target(
    name = cut_change_fig,
    command = make_SEM_figure(sem_results = cut_change_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # run site sem
  tar_target(
    name = cut_change_site,
    command = run_site_SEM(data = cut_change_data,
                           landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_site_out,
    command = summary(cut_change_site)

  ),

  # figure
  tar_target(
    name = cut_change_site_fig,
    command = make_SEM_site_figure(sem_results = cut_change_site_out,
                                   type = "change",
                                   landuse = "cutting",
                                   col = sem_colour)
  ),

  # sem by origin
  # alpine
  tar_target(
    name = cut_change_alp,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_alp_out,
    command = summary(cut_change_alp)

  ),

  # figure
  tar_target(
    name = cut_change_alp_fig,
    command = make_SEM_figure(sem_results = cut_change_alp_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # sub-alpine
  tar_target(
    name = cut_change_sub,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_sub_out,
    command = summary(cut_change_sub)

  ),

  # figure
  tar_target(
    name = cut_change_sub_fig,
    command = make_SEM_figure(sem_results = cut_change_sub_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # Figure 2
  tar_target(
    name = cut_change_figure,
    command = {

      (cut_change_fig + cut_change_site_fig) /
        (cut_change_alp_fig + cut_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_cut_stats,
    command = {

      bind_rows(
        Across = cut_change_out$coefficients,
        Site = cut_change_site_out$coefficients,
        Alpine = cut_change_alp_out$coefficients,
        "Sub-alpine" = cut_change_sub_out$coefficients,
        .id = "Type"
      )

    }
  ),


  # GRAZING
  # prep data
  tar_target(
    name = graz_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)
  ),

  # run sem
  tar_target(
    name = graz_change,
    command = run_SEM(data = graz_change_data,
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_out,
    command = summary(graz_change)

  ),

  # sem grazing figure
  tar_target(
    name = graz_change_fig,
    command = make_SEM_figure(sem_results = graz_change_out,
                                   type = "change",
                                   landuse = "grazing",
                              col = sem_colour)
  ),

  # run site sem
  tar_target(
    name = graz_change_site,
    command = run_site_SEM(data = graz_change_data,
                           landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_site_out,
    command = summary(graz_change_site)

  ),

  # figure
  tar_target(
    name = graz_change_site_fig,
    command = make_SEM_site_figure(sem_results = graz_change_site_out,
                                   type = "change",
                                   landuse = "grazing",
                                   col = sem_colour)
  ),

  # sem by origin
  # alpine
  tar_target(
    name = graz_change_alp,
    command = run_SEM(data = graz_change_data |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_alp_out,
    command = summary(graz_change_alp)

  ),

  # figure
  tar_target(
    name = graz_change_alp_fig,
    command = make_SEM_figure(sem_results = graz_change_alp_out,
                              type = "change",
                              landuse = "grazing",
                              col = sem_colour)
  ),

  # sub-alpine data
  tar_target(
    name = graz_change_sub,
    command = run_SEM(data = graz_change_data |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_sub_out,
    command = summary(graz_change_sub)

  ),

  # figure
  tar_target(
    name = graz_change_sub_fig,
    command = make_SEM_figure(sem_results = graz_change_sub_out,
                              type = "change",
                              landuse = "grazing",
                              col = sem_colour)
  ),

  # Figure 2
  tar_target(
    name = graz_change_figure,
    command = {

      (graz_change_fig + graz_change_site_fig) /
        (graz_change_alp_fig + graz_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_graz_stats,
    command = {

      bind_rows(
        Across = graz_change_out$coefficients,
        Site = graz_change_site_out$coefficients,
        Alpine = graz_change_alp_out$coefficients,
        "Sub-alpine" = graz_change_sub_out$coefficients,
        .id = "Type"
      )

    }
  ),

  tar_target(
    name = div_figure,
    command = make_sem_div(cut_change_data,
                           sem_colour)
  ),

  tar_target(
    name = rich_figure,
    command = make_sem_rich(biomass_div,
                           sem_colour)
  ),

  tar_target(
    name = env_figure,
    command = make_sem_even(biomass_div,
                           sem_colour)
  ),

  
# SEM other diversity indices
  # change in biomass and diversity indices
  # cutting and richness
  tar_target(
    name = cut_change_richness,
    command = {

      # change in richness across site
      dat1 <- prep_SEM_data(data = biomass_div,
                           landuse = "cutting",
                           diversity = log_ratio_richness,
                           biomass = log_ratio_bio)

      mod1 <- run_SEM(data = dat1,
              landuse = "cutting")

      out1 <- summary(mod1)

      # path and estimates
      paths1 <- tibble(from = out1$coefficients$Predictor,
                      to = out1$coefficients$Response,
                      label = round(out1$coefficients$Std.Estimate, 3),
                      P.Value = out1$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
                        to = case_when(to == "biomass" ~ "Δbiomass",
                                       to == "diversity" ~ "Δrichness",
                                       .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '','Δrichness',
                        'warming','', '', '',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

      plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
      fig1 <- plot(plot_model1)


      # with site
      mod2 <- run_site_SEM(data = dat1,
                      landuse = "cutting")

      out2 <- summary(mod2)

      # path and estimates
      paths2 <- tibble(from = out2$coefficients$Predictor,
                       to = out2$coefficients$Response,
                       label = round(out2$coefficients$Std.Estimate, 3),
                       P.Value = out2$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      layout2 = matrix(c('nitrogen', '', '', '',
                        '', 'Δbiomass', '','Δrichness',
                        'warming','', '', 'site',
                        '', 'cutting', '', ''),
                      nrow = 4, byrow = TRUE)

      plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
      fig2 <- plot(plot_model2)


      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")

      out3 <- summary(mod3)

      # path and estimates
      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      # path and estimates
      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      figure <- (fig1 + fig2) /
        (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Across = out1$coefficients,
        Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  # cutting and evenness
  tar_target(
    name = cut_change_evenness,
    command = {

      # change in richness across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = log_ratio_evenness,
                            biomass = log_ratio_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "cutting")

      out1 <- summary(mod1)

      # path and estimates
      paths1 <- tibble(from = out1$coefficients$Predictor,
                       to = out1$coefficients$Response,
                       label = round(out1$coefficients$Std.Estimate, 3),
                       P.Value = out1$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δevenness',
                         'warming','', '', '',
                         '', 'cutting', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
      fig1 <- plot(plot_model1)


      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "cutting")

      out2 <- summary(mod2)

      # path and estimates
      paths2 <- tibble(from = out2$coefficients$Predictor,
                       to = out2$coefficients$Response,
                       label = round(out2$coefficients$Std.Estimate, 3),
                       P.Value = out2$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      layout2 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δevenness',
                         'warming','', '', 'site',
                         '', 'cutting', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
      fig2 <- plot(plot_model2)


      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")

      out3 <- summary(mod3)

      # path and estimates
      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      # path and estimates
      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      figure <- (fig1 + fig2) /
        (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Across = out1$coefficients,
        Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  # grazing and richness
  tar_target(
    name = graz_change_richness,
    command = {

      # change in richness across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_richness,
                            biomass = log_ratio_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "grazing")

      out1 <- summary(mod1)

      # path and estimates
      paths1 <- tibble(from = out1$coefficients$Predictor,
                       to = out1$coefficients$Response,
                       label = round(out1$coefficients$Std.Estimate, 3),
                       P.Value = out1$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δrichness',
                         'warming','', '', '',
                         '', 'grazing', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
      fig1 <- plot(plot_model1)


      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "grazing")

      out2 <- summary(mod2)

      # path and estimates
      paths2 <- tibble(from = out2$coefficients$Predictor,
                       to = out2$coefficients$Response,
                       label = round(out2$coefficients$Std.Estimate, 3),
                       P.Value = out2$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      layout2 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δrichness',
                         'warming','', '', 'site',
                         '', 'grazing', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
      fig2 <- plot(plot_model2)


      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      # path and estimates
      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      # path and estimates
      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δrichness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      figure <- (fig1 + fig2) /
        (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Across = out1$coefficients,
        Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  # grazing and evenness
  tar_target(
    name = graz_change_evenness,
    command = {

      # change in richness across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_evenness,
                            biomass = log_ratio_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "grazing")

      out1 <- summary(mod1)

      # path and estimates
      paths1 <- tibble(from = out1$coefficients$Predictor,
                       to = out1$coefficients$Response,
                       label = round(out1$coefficients$Std.Estimate, 3),
                       P.Value = out1$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δevenness',
                         'warming','', '', '',
                         '', 'grazing', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model1 <- prepare_graph(edges = paths1, layout = layout1)
      fig1 <- plot(plot_model1)


      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "grazing")

      out2 <- summary(mod2)

      # path and estimates
      paths2 <- tibble(from = out2$coefficients$Predictor,
                       to = out2$coefficients$Response,
                       label = round(out2$coefficients$Std.Estimate, 3),
                       P.Value = out2$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      layout2 = matrix(c('nitrogen', '', '', '',
                         '', 'Δbiomass', '','Δevenness',
                         'warming','', '', 'site',
                         '', 'grazing', '', ''),
                       nrow = 4, byrow = TRUE)

      plot_model2 <- prepare_graph(edges = paths2, layout = layout2)
      fig2 <- plot(plot_model2)


      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      # path and estimates
      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      # path and estimates
      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               from = case_when(from == "biomass" ~ "Δbiomass",
                                .default = from),
               to = case_when(to == "biomass" ~ "Δbiomass",
                              to == "diversity" ~ "Δevenness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      figure <- (fig1 + fig2) /
        (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Across = out1$coefficients,
        Site = out2$coefficients,
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
