### Stats tables


round_numbers <- function(table){
  table |>
    mutate(sumsq = round(sumsq, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value))) |>
    select(-names)

}

table_style <- function(gt_table, font_size){

  gt_table |>
    tab_options(
      table.font.size = font_size,
      data_row.padding = gt::px(1)
    ) |>
    cols_align(
      align = c("left"),
      columns = Term
    ) |>
    cols_align(
      align = c("right"),
      columns = contains("p.value")
    )

}

make_biomass_stats <- function(biomass_anova_table){

  round_numbers(biomass_anova_table) |>
    pivot_wider(names_from = origSiteID, values_from = c(sumsq, df, statistic, p.value)) |>
    select(Term = term, sumsq_Alpine, df_Alpine, statistic_Alpine, p.value_Alpine, `sumsq_Sub-alpine`, `df_Sub-alpine`,  `statistic_Sub-alpine`, `p.value_Sub-alpine`) |>
    gt() |>
    tab_spanner(label = "Alpine", columns = c(2:5)) |>
    cols_label(sumsq_Alpine = "Sum of Squares",
               df_Alpine = "df",
               statistic_Alpine = "F",
               p.value_Alpine	= "P") |>
    tab_spanner(label = "Sub-alpine", columns = c(6:9)) |>
    cols_label(`sumsq_Sub-alpine` = "Sum of Squares",
               `df_Sub-alpine` = "df",
               `statistic_Sub-alpine` = "F",
               `p.value_Sub-alpine`	= "P") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_Alpine, df_Alpine, statistic_Alpine, p.value_Alpine),
        rows = p.value_Alpine <= 0.05
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(`sumsq_Sub-alpine`, `df_Sub-alpine`, `statistic_Sub-alpine`, `p.value_Sub-alpine`),
        rows = `p.value_Sub-alpine` <= 0.05
      )
    ) %>%
    table_style(., font_size = 12)

}


make_cover_stats <- function(cover_anova_table){

  dat <- round_numbers(cover_anova_table) |>
    mutate(functional_group = if_else(functional_group %in% c("sedge", "legume"), "other", functional_group)) |>
    select(-group) |>
    pivot_wider(names_from = functional_group, values_from = c(sumsq, df, statistic, p.value)) |>
    mutate(term = factor(term, levels = c("W", "G", "N", "WxG", "WxN", "GxN", "WxGxN", "Residuals"))) |>
    arrange(origSiteID, term) |>
    select(Term = term, sumsq_graminoid, df_graminoid, statistic_graminoid, p.value_graminoid, sumsq_forb, df_forb,  statistic_forb, p.value_forb, sumsq_other, df_other, statistic_other, p.value_other)

  dat |>
    gt() |>
    tab_spanner(label = "Graminoid", columns = c(2:5)) |>
    cols_label(sumsq_graminoid = "Sum of Squares",
               df_graminoid = "df",
               statistic_graminoid = "F",
               p.value_graminoid	= "P") |>
    tab_spanner(label = "Forb", columns = c(6:9)) |>
    cols_label(sumsq_forb = "Sum of Squares",
               df_forb = "df",
               statistic_forb = "F",
               p.value_forb	= "P") |>
    tab_spanner(label = "Other", columns = c(10:13)) |>
    cols_label(sumsq_other = "Sum of Squares",
               df_other = "df",
               statistic_other = "F",
               p.value_other	= "P") |>
    tab_row_group(
      label = "Sub-alpine",
      rows = 9:16
    ) |>
    tab_row_group(
      label = "Alpine",
      rows = 1:8
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_graminoid, df_graminoid, statistic_graminoid, p.value_graminoid),
        rows = p.value_graminoid <= 0.05
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_forb, df_forb, statistic_forb, p.value_forb),
        rows = p.value_forb <= 0.05
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_other, df_other, statistic_other, p.value_other),
        rows = p.value_other <= 0.05
      )
    ) %>%
    table_style(., font_size = 11)

}


make_diversity_stats <- function(diversity_anova_table){

  div <- round_numbers(diversity_anova_table) |>
    pivot_wider(names_from = diversity_index, values_from = c(sumsq, df, statistic, p.value)) |>
    select(Term = term, sumsq_richness, df_richness, statistic_richness, p.value_richness, sumsq_diversity, df_diversity, statistic_diversity, p.value_diversity, sumsq_evenness, df_evenness, statistic_evenness, p.value_evenness)

  div |>
    gt() |>
    tab_spanner(label = "Richness", columns = c(2:5)) |>
    cols_label(sumsq_richness = "Sum of Squares",
               df_richness = "df",
               statistic_richness = "F",
               p.value_richness	= "P") |>
    tab_spanner(label = "Diversity", columns = c(6:9)) |>
    cols_label(sumsq_diversity = "Sum of Squares",
               df_diversity = "df",
               statistic_diversity = "F",
               p.value_diversity	= "P") |>
    tab_spanner(label = "Evenness", columns = c(10:13)) |>
    cols_label(sumsq_evenness = "Sum of Squares",
               df_evenness = "df",
               statistic_evenness = "F",
               p.value_evenness	= "P") |>
    tab_row_group(
      label = "Sub-alpine",
      rows = 9:16
    ) |>
    tab_row_group(
      label = "Alpine",
      rows = 1:8
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_richness, df_richness, statistic_richness, p.value_richness),
        rows = p.value_richness <= 0.05
      )) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_diversity, df_diversity, statistic_diversity, p.value_diversity),
        rows = p.value_diversity <= 0.05
      )) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_evenness, df_evenness, statistic_evenness, p.value_evenness),
        rows = p.value_evenness <= 0.05
      )) %>%
    table_style(., font_size = 11)

}



make_nutrient_stats2 <- function(nutrients_anova_table){

  nutrients <- round_numbers(nutrients_anova_table) |>
    pivot_wider(names_from = origSiteID, values_from = c(sumsq, df, statistic, p.value)) |>
    select(Elements = elements, Term = term, sumsq_Alpine, df_Alpine, statistic_Alpine, p.value_Alpine, `sumsq_Sub-alpine`, `df_Sub-alpine`, `statistic_Sub-alpine`, `p.value_Sub-alpine`) |>
    group_by(Elements)

  nutrients |>
    mutate(Elements = factor(Elements, levels = c("NH4-N", "NO3-N", "P", "K", "Ca", "Mg"))) |>
    arrange(Elements) |>
    gt() |>
    tab_spanner(label = "Alpine", columns = c(2:5)) |>
    cols_label(sumsq_Alpine = "Sum of Squares",
               df_Alpine = "df",
               statistic_Alpine = "F",
               p.value_Alpine	= "P") |>
    tab_spanner(label = "Sub-alpine", columns = c(6:9)) |>
    cols_label(`sumsq_Sub-alpine` = "Sum of Squares",
               `df_Sub-alpine` = "df",
               `statistic_Sub-alpine` = "F",
               `p.value_Sub-alpine`	= "P") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_Alpine, df_Alpine, statistic_Alpine, p.value_Alpine),
        rows = p.value_Alpine <= 0.05
      )) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(`sumsq_Sub-alpine`, `df_Sub-alpine`, `statistic_Sub-alpine`, `p.value_Sub-alpine`),
        rows = `p.value_Sub-alpine` <= 0.05
      ))  %>%
    table_style(., font_size = 11)
}


make_microclimate_stats <- function(daily_temp){

  warming <- daily_temp |>
    mutate(month = month(date),
           year = year(date)) |>
    filter(month %in% c(5, 6, 7, 8, 9),
           grazing == "Control",
           Namount_kg_ha_y == 0)

  warming |>
    group_by(origSiteID, variable) |>
    nest() |>
    mutate(linMod = map(data, ~lm(value ~ warming, data = .)),
           result = map(linMod, tidy)) |>
    unnest(result) |>
    select(-data, -linMod) |>
    mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                            term == "warmingWarming" ~ "Warming")) |>
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = if_else(p.value <= 0.001, "≤ 0.001", as.character(round(p.value, 3)))) |>
    pivot_wider(names_from = origSiteID, values_from = c(estimate, std.error, statistic, p.value)) |> select(Variable = variable, Term = term, "estimate_Sub-alpine" , "std.error_Sub-alpine", "statistic_Sub-alpine", "p.value_Sub-alpine", "estimate_Alpine", "std.error_Alpine", "statistic_Alpine", "p.value_Alpine") |>
    ungroup() |>
    gt() |>
    tab_spanner(label = "Sub-alpine", columns = c(3:6)) |>
    cols_label(`estimate_Sub-alpine` = "Estimate",
               `std.error_Sub-alpine` = "Standard error",
               `statistic_Sub-alpine` = "t",
               `p.value_Sub-alpine`	= "P") |>
    tab_spanner(label = "Alpine", columns = c(7:10)) |>
    cols_label(estimate_Alpine = "Estimate",
               std.error_Alpine = "Standard error",
               statistic_Alpine = "t",
               p.value_Alpine	= "P") %>%
    tab_options(table.font.size = 11)
}






make_climate_stats <- function(climate_anova_table){

  dat <- climate_anova_table |>
    mutate(sumsq = round(sumsq, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           variable = factor(variable, levels = c("air", "ground", "soil", "soilmoisture"))) |>
    select(-names) |>
    pivot_wider(names_from = variable, values_from = c(sumsq, df, statistic, p.value)) |>
    mutate(term = factor(term, levels = c("W", "C", "N", "WxC", "WxN", "NxC", "WxNxC", "Residuals"))) |>
    arrange(origSiteID, term) |>
    select(Term = term, sumsq_air, df_air, statistic_air, p.value_air, sumsq_ground, df_ground,  statistic_ground, p.value_ground, sumsq_soil, df_soil, statistic_soil, p.value_soil, sumsq_soilmoisture, df_soilmoisture, statistic_soilmoisture, p.value_soilmoisture)

  dat |>
    gt() |>
    tab_spanner(label = "Air", columns = c(2:5)) |>
    cols_label(sumsq_air = "Sum of Squares",
               df_air = "df",
               statistic_air = "F",
               p.value_air	= "P") |>
    tab_spanner(label = "Ground", columns = c(6:9)) |>
    cols_label(sumsq_ground = "Estimate ± SE",
               df_ground = "df",
               statistic_ground = "t",
               p.value_ground	= "P") |>
    tab_spanner(label = "Soil", columns = c(10:13)) |>
    cols_label(sumsq_soil = "Estimate ± SE",
               df_soil = "df",
               statistic_soil = "t",
               p.value_soil	= "P") |>
    tab_spanner(label = "Soilmoisture", columns = c(14:17)) |>
    cols_label(sumsq_soilmoisture = "Estimate ± SE",
               df_soilmoisture = "df",
               statistic_soilmoisture = "t",
               p.value_soilmoisture	= "P") |>
    tab_row_group(
      label = "Sub-alpine",
      rows = 9:16
    ) |>
    tab_row_group(
      label = "Alpine",
      rows = 1:8
    ) |>
    tab_options(
      table.font.size = 11,
      data_row.padding = gt::px(1)
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_air, df_air, statistic_air, p.value_air),
        rows = p.value_air <= 0.05
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_ground, df_ground, statistic_ground, p.value_ground),
        rows = p.value_ground <= 0.05
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_soil, df_soil, statistic_soil, p.value_soil),
        rows = p.value_soil <= 0.05
      )
    )  |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(sumsq_soilmoisture, df_soilmoisture, statistic_soilmoisture, p.value_soilmoisture),
        rows = p.value_soilmoisture <= 0.05
      )
    ) |>
    cols_align(
      align = c("left"),
      columns = Term
    )

}

make_trait_stats <- function(trait_statistical_analysis){

  # Unnest the anova_tidy results and format
  trait_stats <- trait_statistical_analysis |>
    unnest(anova_tidy) |>
    mutate(
      sumsq = round(sumsq, 2),
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value)),
      # Rename terms to be more readable
      term = case_when(
        term == "biomass_log" ~ "Log(Standing biomass)",
        term == "grazing_num" ~ "Clipping",
        term == "Nitrogen_log" ~ "Log(Nitrogen addition)",
        term == "warmingWarming" ~ "Warming",
        TRUE ~ term
      )
    ) |>
    select(trait_trans, trait_fancy, origSiteID, treatment, term, sumsq, df, statistic, p.value) |>
    # Pivot to wide format with origSiteID as columns
    pivot_wider(
      names_from = origSiteID, 
      values_from = c(sumsq, df, statistic, p.value),
      names_glue = "{origSiteID}_{.value}"
    ) |>
    ungroup() |>
    # Reorder columns to group by site
    select(
      treatment, trait_fancy, Term = term,
      # Alpine columns
      Alpine_sumsq, Alpine_df, Alpine_statistic, Alpine_p.value,
      # Sub-alpine columns  
      `Sub-alpine_sumsq`, `Sub-alpine_df`, `Sub-alpine_statistic`, `Sub-alpine_p.value`
    )

  # Create gt table
  trait_stats |>
    gt() |>
    # Add spanners for each site
    tab_spanner(label = "Alpine", columns = c(4:7)) |>
    tab_spanner(label = "Sub-alpine", columns = c(8:11)) |>
    # Label columns consistently
    cols_label(
      trait_fancy = "Trait",
      # Alpine
      Alpine_sumsq = "Sum of Squares",
      Alpine_df = "df", 
      Alpine_statistic = "F",
      Alpine_p.value = "P",
      # Sub-alpine
      `Sub-alpine_sumsq` = "Sum of Squares",
      `Sub-alpine_df` = "df",
      `Sub-alpine_statistic` = "F", 
      `Sub-alpine_p.value` = "P"
    ) |>
    # Add row groups by treatment (reordered)
    tab_row_group(
      label = "Warming",
      rows = treatment == "warming"
    ) |>
    tab_row_group(
      label = "Nitrogen",
      rows = treatment == "nitrogen"
    ) |>
    tab_row_group(
      label = "Clipping", 
      rows = treatment == "grazing"
    ) |>
    tab_row_group(
      label = "Biomass",
      rows = treatment == "biomass"
    ) |>
    # Set the order of row groups
    row_group_order(groups = c("Warming", "Nitrogen", "Clipping", "Biomass")) |>
    # Bold significant p-values for each site
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = c(Alpine_sumsq, Alpine_df, Alpine_statistic, Alpine_p.value),
        rows = Alpine_p.value <= 0.05
      )
    ) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = c(`Sub-alpine_sumsq`, `Sub-alpine_df`, `Sub-alpine_statistic`, `Sub-alpine_p.value`),
        rows = `Sub-alpine_p.value` <= 0.05
      )
    ) |>
    # Apply consistent styling
    table_style(font_size = 11) |>
    # Hide trait_trans and treatment columns since they're in row groups
    cols_hide(c(treatment))
}
