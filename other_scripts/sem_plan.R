## structural equation model

sem_plan <- list(

    # run sem for drivers, biomass and diversity
    tar_target(
      name = sem0_fit,
      command = {

        # All data: drivers, biomass and diversity
        # biomass collected
        model <- '
    # regressions
      diversity ~ biomass + warming + nitrogen + grazing
      biomass ~ warming + nitrogen + grazing
  '

        dat <- biomass_div |>
          filter(grazing != "Natural",
                 biomass_remaining_coll > 0) |>
          mutate(warming = if_else(warming == "Ambient", 0, 1),
                 nitrogen = Nitrogen_log,
                 grazing = grazing_num,
                 biomass = log(biomass_remaining_coll))

        sem(model, data = dat)

      }
    ),

    # plot sem
    tar_target(
      name = sem0_plot,
      command = {

        lay <- get_layout(NA, "warming", NA, NA,
                          "nitrogen", NA, NA, NA,
                          NA, NA, "biomass", "diversity",
                          NA, NA, NA, NA,
                          NA, "grazing", NA, NA,
                          rows = 5)

        plot_sem(fit = sem0_fit, lay = lay)
      }
    ),

    tar_target(
      name = sem_fit,
      command = {
        # All data: drivers, biomass and diversity
        # biomass estimated
        model <- '
    # regressions
      diversity ~ biomass + warming + nitrogen + grazing
      biomass ~ warming + nitrogen + grazing
  '

        dat <- biomass_div |>
          mutate(warming = if_else(warming == "Ambient", 0, 1),
                 nitrogen = Nitrogen_log,
                 grazing = grazing_num,
                 biomass = log(biomass_remaining_calc))

        sem(model, data = dat)

      }
    ),

    tar_target(
      name = sem_fit_out,
      command = tidy(sem_fit) |>
        slice(1:7) |>
        select(term, estimate, std.error, statistic, p.value) |>
        mutate(estimate = round(estimate, 2),
               std.error = round(std.error, 2),
               p.value = round(p.value, 3))

    ),

    # plot sem
    tar_target(
      name = sem_plot,
      command = {

        lay <- get_layout(NA, "warming", NA, NA,
                          "nitrogen", NA, NA, NA,
                          NA, NA, "biomass", "diversity",
                          NA, NA, NA, NA,
                          NA, "grazing", NA, NA,
                          rows = 5)

        plot_sem(fit = sem_fit, lay = lay)
      }
    ),

    #varTable(fit)
    #summary(fit, standardized = TRUE)

  # separate by origin
  tar_target(
    name = sem_fit_alp,
    command = {

      # Alpine: drivers, biomass and diversity
      model <- '
    # regressions
      diversity ~ biomass + warming + nitrogen + grazing
      biomass ~ warming + nitrogen + grazing
  '

      sem(mod = model, data = biomass_div |>
            filter(origSiteID == "Alpine")|>
            mutate(nitrogen = Nitrogen_log,
                   grazing = grazing_num,
                   biomass = log(biomass_remaining_calc)))

    }
  ),

  tar_target(
    name = sem_fit_sub,
    command = {

      # Sub-alpine: drivers, biomass and diversity
      model <- '
    # regressions
      diversity ~ biomass + warming + nitrogen + grazing
      biomass ~ warming + nitrogen + grazing
  '

      sem(mod = model, data = biomass_div |>
                   filter(origSiteID == "Sub-alpine")|>
            mutate(nitrogen = Nitrogen_log,
                   grazing = grazing_num,
                   biomass = log(biomass_remaining_calc)))

    }
  ),

  # plot sem
  tar_target(
    name = sem_plot_origin,
    command = {

      lay <- get_layout(NA, "warming", NA, NA,
                        "nitrogen", NA, NA, NA,
                        NA, NA, "biomass", "diversity",
                        NA, "grazing", NA, NA,
                        rows = 4)

      a <- plot_sem(fit = sem_fit_alp, lay = lay)
      s <- plot_sem(fit = sem_fit_sub, lay = lay)

      a/s
    }
  ),


  # run sem for drivers, biomass and diversity
  tar_target(
    name = sem_fit_prod,
    command = {

      model <- '
    # regressions
      diversity ~ biomass + warming + nitrogen + grazing
      productivity ~ biomass + warming + nitrogen + grazing
      biomass ~ productivity + grazing
  '

      dat <- biomass_div |>
        filter(grazing != "Natural") |>
        mutate(warming = if_else(warming == "Ambient", 0, 1),
               nitrogen = Nitrogen_log,
               grazing = grazing_num,
               biomass = log(biomass_remaining_calc),
               productivity = prod_scl)

      sem_fit_prod <- sem(mod = model, data = dat)

    }
  ),

  tar_target(
    name = sem_fit_prod_out,
    command = tidy(sem_fit_prod) |>
      slice(1:9) |>
      select(term, estimate, std.error, statistic, p.value) |>
      mutate(estimate = round(estimate, 2),
             std.error = round(std.error, 2),
             p.value = round(p.value, 3))

  ),

  # plot sem
  tar_target(
    name = sem_plot_prod,
    command = {

      lay <- get_layout(NA, "warming", "nitrogen",
                        NA, "productivity", NA,
                        NA, "biomass", "diversity",
                        "grazing", NA, NA,
                        rows = 4)
      plot_sem(fit = sem_fit_prod, lay = lay)
    }
  )


)

# model1 <- '
#     # regressions
#       diversity ~ biomass + warming + nitrogen + grazing
#       productivity ~ biomass + warming + nitrogen + grazing
#       biomass ~ productivity + grazing
#   '
#
# fit1 <- sem(mod = model1, data = biomass_div |>
#       mutate(nitrogen = Nitrogen_log,
#              grazing = grazing_num,
#              biomass = log(biomass_remaining_calc),
#              productivity = prod_scl))
#
#
# model2 <- '
#     # regressions
#       diversity ~ biomass + warming + nitrogen + grazing
#       productivity ~ warming + nitrogen
#       biomass ~ productivity + grazing
#   '
#
# fit2 <- sem(mod = model2, data = biomass_div |>
#               mutate(nitrogen = Nitrogen_log,
#                      grazing = grazing_num,
#                      biomass = log(biomass_remaining_calc),
#                      productivity = prod_scl))
#
# AIC(fit1, fit2)

# bio <- biomass_raw %>%
#   # calculate area in m2 and scale biomass to m2
#   mutate(area_m2 = area_cm2 / 10000,
#          biomass_scaled = biomass / area_m2
#   ) %>%
#
#   # log transform Nitrogen
#   mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
#
#   # prettify
#   mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
#          origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
#          grazing = factor(grazing, levels = c("C", "M", "I", "N")),
#          grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
#          warming = recode(warming, "A" = "Ambient", "W" = "Warming")) |>
#   filter(!fun_group %in% c("litter")) |>
#   # sum all functional groups
#   group_by(origSiteID, destSiteID, turfID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year, cut) |>
#   summarise(biomass = sum(biomass_scaled))
#
# library(performance)
# bio21 <- bio |>
#   filter(year == "2021",
#          cut == 3,
#          Namount_kg_ha_y != 150,
#          grazing %in% c("Control", "Natural")) |>
#   ungroup() |>
#   select(origSiteID, destSiteID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, biomass) |>
#   pivot_wider(names_from = grazing, values_from = biomass)
#
# fit <- lm(log(Natural) ~ log(Control) + warming * Nitrogen_log + origSiteID, data = bio21)
# summary(fit)
# check_model(fit)
#
# ggplot(bio21, aes(x = log(Control), y = log(Natural), size = Nitrogen_log)) +
#   geom_point() +
#   facet_grid(warming ~ origSiteID)
#
# bio22 <- bio |>
#   ungroup() |>
#   filter(year == "2022",
#          cut == 3,
#          Namount_kg_ha_y != 150,
#          grazing %in% c("Control")) |>
#   select(origSiteID, destSiteID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, biomass) |>
#   pivot_wider(names_from = grazing, values_from = biomass) |>
#   mutate(Natural = NA)
#
#
# bio22 <- bio22 %>%
#   mutate(Natural_log = predict(object = fit, newdata = .),
#          Natural = exp(Natural_log))
# bind_rows(
#   y2021 = bio21,
#   y2022 = bio22,
#   .id = "year"
# ) |>
#   mutate(year = as.numeric(if_else(year == "y2021", 2021, 2022))) |>
#   ggplot(aes(x = Control, y = Natural, size = Nitrogen_log, colour = as.factor(year))) +
#   geom_point() +
#   facet_grid(warming ~ origSiteID)
#
#
# ### test 2021
# standing_biomass21 <- biomass_raw %>%
#   # calculate area in m2 and scale biomass to m2
#   mutate(area_m2 = area_cm2 / 10000,
#          biomass_scaled = biomass / area_m2
#   ) %>%
#   # only useful data for last 2 years
#   filter(year %in% c(2021)) %>%
#
#   # log transform Nitrogen
#   mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
#
#   # prettify
#   mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
#          origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
#          grazing = factor(grazing, levels = c("C", "M", "I", "N")),
#          grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
#          warming = recode(warming, "A" = "Ambient", "W" = "Warming")) |>
#
#   # make grazing numeric
#   mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
#          grazing_num = as.numeric(grazing_num))
#
# bio21_remain <- standing_biomass21 |>
#   filter(fun_group != "litter") |>
#   ungroup() |>
#   group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing) |>
#   summarise(biomass = sum(biomass)) |>
#   pivot_wider(names_from = grazing, values_from = biomass) |>
#   mutate(Medium = Control - Medium,
#          Intensive = Control - Intensive) |>
#   pivot_longer(cols = c(Control, Medium, Intensive, Natural), names_to = "grazing", values_to = "biomass_remaining")
#
# dat <- diversity |>
#   filter(grazing != "Natural") |>
#   left_join(bio_remain,
#             by = c('origSiteID', "grazing", 'warming', 'Namount_kg_ha_y', 'Nitrogen_log')) |>
#   filter(origSiteID == "Alpine",
#          diversity_index == "diversity") |>
#   ungroup() |>
#   select(warming, nitrogen = Nitrogen_log, biomass_remaining, grazing, diversity = `2022`) |>
#   mutate(biomass_scl = scale(biomass_remaining))
#
#
#
# model <- '
#   # regressions
#     diversity ~ warming + nitrogen + biomass_scl
#     biomass_scl ~ warming + nitrogen
# '
#
# fit <- sem(model, data = dat)
# varTable(fit)
# summary(fit, standardized = TRUE)
# graph_sem(model = fit)
# get_edges(fit)
# get_nodes(fit)
#
# lay <- get_layout("warming", NA,  NA,
#                   "nitrogen", NA, NA,
#                   NA, "biomass_scl", "diversity",
#                   #"grazing", NA, NA,
#                   rows = 3)
#
# graph_data <- prepare_graph(model = fit, layout = lay)
# plot(graph_data)
#
#
#
# ### 2022
# consumption |>
#   select(siteID, Consumption) |>
#   group_by(siteID) |>
#   summarise(c = mean(Consumption) / 1000)
#
# bio_remain <- standing_biomass |>
#   filter(fun_group != "litter") |>
#   ungroup() |>
#   group_by(origSiteID, destSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing) |>
#   summarise(biomass = sum(biomass)) |>
#   pivot_wider(names_from = grazing, values_from = biomass) |>
#   mutate(Medium = Control - Medium,
#          Intensive = Control - Intensive) |>
#   pivot_longer(cols = c(Control, Medium, Intensive), names_to = "grazing", values_to = "biomass_remaining")
#
# dat <- diversity |>
#   filter(grazing != "Natural") |>
#   left_join(bio_remain,
#             by = c('origSiteID', "grazing", 'warming', 'Namount_kg_ha_y', 'Nitrogen_log')) |>
#   filter(origSiteID == "Sub-alpine",
#          diversity_index == "diversity") |>
#   ungroup() |>
#   select(origSiteID, warming, nitrogen = Nitrogen_log, biomass_remaining, grazing, diversity = `2022`) |>
#   mutate(biomass_scl = scale(biomass_remaining))
#
# ggplot(dat, aes(x = biomass_remaining, y = diversity, colour = warming)) +
#   geom_point() +
#   facet_wrap(~ origSiteID)
#
# fit <- lm(diversity ~ biomass_remaining + origSiteID + warming, dat)
# summary(fit)
#
# model <- '
#   # regressions
#     diversity ~ warming + nitrogen + grazing + biomass_scl
#     biomass_scl ~ warming + nitrogen + grazing
# '
#
# fit <- sem(model, data = dat)
# varTable(fit)
# summary(fit, standardized = TRUE)
# graph_sem(model = fit)
# get_edges(fit)
# get_nodes(fit)
#
# lay <- get_layout("warming", NA,  NA,
#            "nitrogen", NA, NA,
#            NA, "biomass_scl", "diversity",
#            "grazing", NA, NA, rows = 4)
#
# graph_data <- prepare_graph(model = fit, layout = lay)
# plot(graph_data)
#
#
#
#
#
# model <- '
#   # regressions
#     diversity ~ warming + nitrogen + grazing + biomass_scl
#     biomass_scl ~ warming + nitrogen + grazing
# '
#
# fit <- sem(model, data = dat)
# varTable(fit)
# summary(fit, standardized = TRUE)
# graph_sem(model = fit)
# get_edges(fit)
# get_nodes(fit)
#
# lay <- get_layout("warming", NA,  NA,
#                   "nitrogen", NA, NA,
#                   NA, "biomass_scl", "diversity",
#                   "grazing", NA, NA, rows = 4)
#
# graph_data <- prepare_graph(model = fit, layout = lay)
# plot(graph_data)
#
#
# # estimate biomass for natural grazed plots
# dd <- biomass_est |>
#   tidylog::left_join(biomass |>
#                        # get peak biomass and remove litter
#                        filter(grazing == "Control") |>
#                        #filter(grazing == "Control" & cut == 3| grazing %in% c("Medium", "Intensive") & cut == 4) |>
#                        filter(!fun_group %in% c("litter")) |>
#
#                        # Calculate mean of 0 kg N per m2 y
#                        ungroup() |>
#                        group_by(origSiteID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num) |>
#                        summarise(biomass = mean(biomass_scaled)))
#
# fit <- lm(biomass ~ 0 + biomass_calc, data = dd)
# summary(fit)
# ggplot(dd, aes(x = biomass_calc, y = biomass)) +
#   geom_point()
