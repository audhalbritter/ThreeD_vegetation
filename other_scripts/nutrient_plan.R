# nutrient availability data

nutrient_plan <- list(

  # download
  tar_target(
    name = prs_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_nutrients_2021.csv",
                        path = "data",
                        remote_path = "Soil"),
    format = "file"
  ),

  tar_target(
    name = prs_raw,
    command = read_csv(file = prs_download)
  ),

  # soil nutrient supply in situ.
  tar_target(
    name = prs_pretty,
    command = prs_raw |>
      filter(elements %in% c("NH4-N", "NO3-N", "P", "Ca", "K", "Mg")) |>
      #filter(!elements %in% c("Cu", "Pb")) |>
      # prettify
      mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
             origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),
             grazing = factor(grazing, levels = c("C", "M", "I")),
             grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive"),
             warming = recode(warming, "A" = "Ambient", "W" = "Warming")) |>
      # make grazing numeric
      mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
             grazing_num = as.numeric(grazing_num)) |>
      # log transform Nitrogen
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
      # fancy names
      mutate(elements_fancy = case_match(elements, "NH4-N" ~ "NH^{4}-N",
                                         "NO3-N" ~ "NO[3]-N",
                                         .default = elements))
  ),

  # nutrient availability model
  tar_target(
    name = nutrient_model,
    command = run_full_model(dat = prs_pretty,
                             group = c("origSiteID", "elements"),
                             response = value,
                             grazing_var = grazing_num) |>
      # make long table
      pivot_longer(cols = -c(origSiteID, elements, data),
                   names_sep = "_",
                   names_to = c(".value", "effects", "names")) |>
      unnest(glance) |>
      filter(effects == "interaction",
             names != "quadratic") |>
      select(origSiteID:adj.r.squared, AIC, deviance) |>
      filter(AIC == min(AIC))
  ),

  # make predictions
  tar_target(
    name = nutrient_output,
    command = make_prediction(nutrient_model) |>
      mutate(elements = factor(elements, levels = c("NH4-N", "NO3-N", "P", "K", "Ca", "Mg")))
  ),

  # stats
  tar_target(
    name = nutrients_anova_table,
    command = nutrient_output |>
      select(origSiteID, elements, names, anova_tidy) |>
      unnest(anova_tidy) |>
      ungroup() |>
      fancy_stats()
  ),

 # pretty stats
  tar_target(
    name = nutrient_summary_table,
    command = nutrient_output |>
      select(origSiteID, elements, names, result) |>
      unnest(result) |>
      ungroup() |>
      fancy_stats()
  ),

  # prepare model output
  tar_target(
    name = nutrient_prediction,
    command = nutrient_output |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(origSiteID, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(elements = factor(elements, levels = c("NH4-N", "NO3-N", "P", "K", "Ca", "Mg")))
  ),

  # make figure
  tar_target(
    name = nutrient_figure,
    command = {

      nutrient_text <- nutrients_anova_table |>
        mutate(significance = case_when(p.value >= 0.05 ~ "non-sign",
                                        term == "Residuals" ~ "non-sign",
                                        TRUE ~ "sign")) |>
        filter(significance == "sign") |>
        distinct(origSiteID, elements, term) |>
        mutate(x_pos = if_else(elements %in% c("NH4-N", "NO3-N"), -Inf, Inf),
               hjust_var = if_else(elements %in% c("NH4-N", "NO3-N"), 0, 1),
               vjust_var = if_else(elements %in% c("Ca", "Mg"), 1, 0))

      top_text <- nutrient_text |>
        filter(!elements %in% c("Ca", "Mg"))

      bottom_text <- nutrient_text |>
        filter(elements %in% c("Ca", "Mg"))

      make_vegetation_figure(dat1 = nutrient_output |>
                               unnest(data),
                             x_axis = Nitrogen_log,
                             yaxislabel = bquote(Available~nutrients~μg~cm^-2~35~days^-1),
                             colourpalette = col_palette,
                             linetypepalette = c("solid", "dashed", "dotted"),
                             shapepalette = c(16, 0, 2),
                             facet_2 = "elements_fancy",
                             dat2 = nutrient_prediction) +
        facet_grid2(origSiteID ~ elements, scales = "free_y", independent = "y", labeller = label_parsed) +
        # add stats
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(top_text |>
                                group_by(origSiteID, elements) |>
                                slice(1), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = Inf, hjust = hjust_var, vjust = 1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(top_text |>
                                group_by(origSiteID, elements) |>
                                slice(2), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = Inf, hjust = hjust_var, vjust = 3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(top_text |>
                                group_by(origSiteID, elements) |>
                                slice(3), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = Inf, hjust = hjust_var, vjust = 4.6, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(top_text |>
                                group_by(origSiteID, elements) |>
                                slice(4), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = Inf, hjust = hjust_var, vjust = 6.2, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(bottom_text |>
                                group_by(origSiteID, elements) |>
                                slice(2), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = -Inf, hjust = hjust_var, vjust = -1.4, label = term),
                  size = 3, colour = text_colour, nudge_x = 50) +
        geom_text(data = nutrient_prediction |>
                    distinct(origSiteID, elements, warming, Namount_kg_ha_y, grazing) |>
                    left_join(bottom_text |>
                                group_by(origSiteID, elements) |>
                                slice(1), by = c("origSiteID", "elements")),
                  aes(x = x_pos, y = -Inf, hjust = hjust_var, vjust = -3, label = term),
                  size = 3, colour = text_colour, nudge_x = 50)

    }
  )


)


# annotations <- data.frame(
#   xpos = c(-Inf,-Inf,Inf,Inf),
#   ypos =  c(-Inf, Inf,-Inf,Inf),
#   annotateText = c("Bottom Left (h0,v0)","Top Left (h0,v1)"
#                    ,"Bottom Right h1,v0","Top Right h1,v1"),
#   hjustvar = c(0,0,1,1) ,
#   vjustvar = c(0,1,0,1))

# make_vegetation_figure(dat1 = nutrient_output |>
#                          unnest(data) |>
#                          filter(origSiteID == "Sub-alpine"),
#                        x_axis = Nitrogen_log,
#                        yaxislabel = bquote(Available~~nutrients~μg~cm^-2~time-1),
#                        colourpalette = col_palette,
#                        linetypepalette = c("solid", "dashed", "dotted"),
#                        shapepalette = c(16, 0, 2),
#                        facet_2 = "elements",
#                        dat2 = nutrient_prediction |>
#                          filter(origSiteID == "Sub-alpine")) +
#   facet_wrap(~ elements, scales = "free")

