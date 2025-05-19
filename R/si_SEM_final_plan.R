# si SEM plan

si_SEM_final_plan <- list(

  # cutting and richness
  tar_target(
    name = cut_final_richness,
    command = {

      # final diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = final_richness,
                            biomass = final_bio)

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
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'biomass', '','richness',
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
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      layout2 =  matrix(c('nitrogen', '', '', '',
                          '', 'biomass', '','richness',
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

      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      #figure <- (fig1 + fig2) / (fig3 + fig4) +
      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
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
    name = cut_final_evenness,
    command = {

      # change in diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = final_evenness,
                            biomass = final_bio)

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
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'biomass', '','evenness',
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
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      layout2 =  matrix(c('nitrogen', '', '', '',
                          '', 'biomass', '','evenness',
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

      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      #figure <- (fig1 + fig2) /
      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
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



  # grazing and diversity
  tar_target(
    name = graz_final_diversity,
    command = {

      # change in diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_diversity,
                            biomass = final_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "grazing")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "grazing",
                              col = sem_colour)

      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "grazing")

      out2 <- summary(mod2)

      fig2 <- make_SEM_site_figure(sem_results = out2,
                                   type = "final",
                                   landuse = "grazing",
                                   col = sem_colour)

      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "grazing",
                              col = sem_colour)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "grazing",
                              col = sem_colour)

      #figure <- (fig1 + fig2) /
      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
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

  # cutting and richness
  tar_target(
    name = graz_final_richness,
    command = {

      # change in diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_richness,
                            biomass = final_bio)

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
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'biomass', '','richness',
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
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      layout2 =  matrix(c('nitrogen', '', '', '',
                          '', 'biomass', '','richness',
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

      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "richness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      #figure <- (fig1 + fig2) /
      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
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
    name = graz_final_evenness,
    command = {

      # change in diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_evenness,
                            biomass = final_bio)

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
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      layout1 = matrix(c('nitrogen', '', '', '',
                         '', 'biomass', '','evenness',
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
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      layout2 =  matrix(c('nitrogen', '', '', '',
                          '', 'biomass', '','evenness',
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

      paths3 <- tibble(from = out3$coefficients$Predictor,
                       to = out3$coefficients$Response,
                       label = round(out3$coefficients$Std.Estimate, 3),
                       P.Value = out3$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      plot_model3 <- prepare_graph(edges = paths3, layout = layout1)
      fig3 <- plot(plot_model3)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      paths4 <- tibble(from = out4$coefficients$Predictor,
                       to = out4$coefficients$Response,
                       label = round(out4$coefficients$Std.Estimate, 3),
                       P.Value = out4$coefficients$P.Value) |>
        mutate(linetype = if_else(P.Value <= 0.05, 1, 2),
               colour = if_else(label > 0, sem_colour[1], sem_colour[2]),
               size = case_when(P.Value <= 0.05 ~ 1.5,
                                P.Value > 0.05 & P.Value <= 0.09 ~ 1,
                                TRUE ~ 0.5),
               to = case_when(to == "diversity" ~ "evenness",
                              .default = to))

      plot_model4 <- prepare_graph(edges = paths4, layout = layout1)
      fig4 <- plot(plot_model4)

      #figure <- (fig1 + fig2) /
      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine site', 'b) Sub-alpine site'))) &
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
