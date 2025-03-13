# make analysis
si_analysis_plan <- list(

  ## ESTIMATE STANDING BIOMASS
  tar_target(
    name = standing_biomass_data,
    command = estimated_standing_biomass |>
        select(-sum_cover, -height) |>
        filter(year == 2022) |>
        # join collected biomass from control plots
        tidylog::left_join(measured_standing_biomass |>
                             filter(grazing == "Control"))
  ),

  tar_target(
    name = standing_biomass_model,
    command = {
      # Linear model
      fit <- lm(biomass_remaining_coll ~ biomass_remaining_calc + Nitrogen_log, data = standing_biomass_data |>
                  filter(grazing == "Control",
                         year == 2022))
    }
  ),

  tar_target(
    name = standing_biomass_model_output,
    command = summary(SB_back_model_22)
  ),


  ## MICROCLIMATE
  # run 3-way interaction model for climate
  tar_target(
    name = climate_model,
    command = {

      average_summer_climate <- daily_temp |>
        mutate(month = month(date),
               year = year(date)) |>
        filter(month %in% c(5, 6, 7, 8, 9)) |>
        group_by(variable, origSiteID, warming, grazing, Namount_kg_ha_y) |>
        summarise(value = mean(value)) |>
        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
               grazing_num = as.numeric(grazing_num)) |>
        # log transform Nitrogen
        mutate(Nitrogen_log = log(Namount_kg_ha_y + 1))

      run_full_model(dat = average_summer_climate |>
                       filter(grazing != "Natural"),
                     group = c("origSiteID", "variable"),
                     response = value,
                     grazing_var = grazing_num) |>
        # make long table
        pivot_longer(cols = -c(origSiteID, variable, data),
                     names_sep = "_",
                     names_to = c(".value", "effects", "names")) |>
        unnest(glance) |>
        select(variable:adj.r.squared, AIC) |>
        # select only interaction models
        filter(effects == "interaction") |>
        # select best model (BY HAND!!!)
        filter(names == "log")
      #filter(AIC == min(AIC)) # normally one would do this

    }

  ),

  # prediction and model output
  tar_target(
    name = climate_output,
    command = make_prediction(climate_model)

  ),

  # prepare model output
  tar_target(
    name = climate_prediction,
    command = climate_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, variable, output) |>
      unnest(output) |>
      rename(prediction = fit) #|>
    # mutate(functional_group = factor(functional_group, levels = c("graminoid", "forb", "sedge", "legume")))
  ),


  # stats
  tar_target(
    name =   climate_anova_table,
    command = climate_output |>
      select(origSiteID, variable, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = climate_summary_table,
    command = climate_output |>
      select(origSiteID, variable, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  tar_target(
    name = microclimate_stats,
    command = make_microclimate_stats(daily_temp)
  ),

  tar_target(
    name = microclimate_save,
    command = microclimate_stats |>
      gtsave("output/microclimate_stats.png", expand = 10)
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
  ),


  # Final biomass and diversity indices
  # cutting and diversity
  tar_target(
    name = cut_final_diversity,
    command = {

      # change in diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = final_diversity,
                            biomass = final_bio)

      mod1 <- run_SEM(data = dat1,
                      landuse = "cutting")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # with site
      mod2 <- run_site_SEM(data = dat1,
                           landuse = "cutting")

      out2 <- summary(mod2)

      fig2 <- make_SEM_site_figure(sem_results = out2,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "cutting",
                              col = sem_colour)

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

  # cutting and richness
  tar_target(
    name = cut_final_richness,
    command = {

      # change in diversity across site
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

