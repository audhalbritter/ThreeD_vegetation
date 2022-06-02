# make analysis
analysis_plan <- list(

  # annual climate
  tar_target(
    name = site_climate,
    command = annual_climate |>
      group_by(siteID, variable) |>
      summarise(se = sd(value)/sqrt(n()),
                mean = mean(value))
  ),

  # summer temp (note that temp recording in 2019 starts in August! Temp is too low.)
  tar_target(
    name = mean_summer_climate,
    command = daily_temp |>
      mutate(month = month(date),
             year = year(date)) %>%
      filter(month %in% c(6, 7, 8, 9),
             year %in% c(2020, 2021),
             # only controls
             Nlevel %in% c(1, 2, 3),
             grazing == "C") %>%
      group_by(year, variable, origSiteID, warming) %>%
      summarise(mean = mean(value),
                se = sd(value)/sqrt(n())) |>
      pivot_wider(names_from = warming, values_from = c(mean, se)) |>
      mutate(diff = mean_Warming - mean_Ambient) |>
      mutate(destSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) |>
      arrange(year, variable, destSiteID)
  ),


  # cover
  tar_target(
    name = cover_model_selection,
    command = do_model_selection(functional_group_cover)
    ),

  tar_target(
    name = cover_result,
    command = {

      dat <- functional_group_cover |>
        filter(grazing != "Natural",
               functional_group %in% c("graminoid", "forb")) |>
        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = "2"),
               grazing_num = as.numeric(grazing_num)) |>

        # best model
        mutate(best_model = case_when(origSiteID == "Alpine" & functional_group == "graminoid" ~ "delta ~ grazing_num * warming + Nitrogen_log * warming",
                                      origSiteID == "Alpine" & functional_group == "forb" ~ "delta ~ Nitrogen_log * warming",
                                      origSiteID == "Sub-alpine" & functional_group == "graminoid" ~ "delta ~ grazing_num * Nitrogen_log + grazing_num * warming",
                                      origSiteID == "Sub-alpine" & functional_group == "forb" ~ "delta ~ Nitrogen_log * warming")) |>
        group_by(origSiteID, functional_group, best_model)

        cover_result <- run_best_models(dat)

    }
  ),

  tar_target(
    name = cover_cn_model_selection,
    command = do_cn_model_selection(functional_group_cover)
  ),

  tar_target(
    name = cover_cn_result,
    command = {

      dat <- functional_group_cover |>
        filter(grazing %in% c("Control", "Natural"),
             functional_group %in% c("graminoid", "forb")) |>

        # best model
        mutate(best_model = case_when(origSiteID == "Alpine" & functional_group == "graminoid" ~ "delta ~ Nitrogen_log * warming",
                                      origSiteID == "Alpine" & functional_group == "forb" ~ "delta ~ grazing * Nitrogen_log + warming",
                                      origSiteID == "Sub-alpine" & functional_group == "graminoid" ~ "delta ~ Nitrogen_log * warming",
                                      origSiteID == "Sub-alpine" & functional_group == "forb" ~ "delta ~ Nitrogen_log * warming",
                                      TRUE ~ "delta ~ Nitrogen_log * warming")) |>
        group_by(origSiteID, functional_group, best_model)

      cover_cn_result <- run_best_models(dat)

    }
  ),


  tar_target(
    name = diversity_result,
    command = {

      dat <- diversity |>
        filter(grazing != "Natural") |>
        # make grazing numeric
        mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4", Natural = "2"),
               grazing_num = as.numeric(grazing_num)) |>

        # best model
        mutate(best_model = case_when(origSiteID == "Alpine" & diversity_index == "richness" ~ "delta ~ Nitrogen_log + warming",
                                      origSiteID == "Sub-alpine" & diversity_index == "richness" ~ "delta ~ Nitrogen_log * warming",
                                      origSiteID == "Alpine" & diversity_index == "diversity" ~ "delta ~ Nitrogen_log * warming",
                                      origSiteID == "Sub-alpine" & diversity_index == "diversity" ~ "delta ~ grazing_num + Nitrogen_log * warming",
                                      origSiteID == "Alpine" & diversity_index == "evenness" ~ "delta ~ Nitrogen_log",
                                      origSiteID == "Sub-alpine" & diversity_index == "evenness" ~ "delta ~ grazing_num + Nitrogen_log")) |>
        group_by(origSiteID, diversity_index, best_model)

      diversity_result <- run_best_models(dat)

    }
  )


)
