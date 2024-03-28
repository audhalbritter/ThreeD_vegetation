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



make_nutrient_stats <- function(nutrients_anova_table){

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



make_climate_stats <- function(climate_anova_table){

  climate <- round_numbers(climate_anova_table) |>
    pivot_wider(names_from = origSiteID, values_from = c(sumsq, df, statistic, p.value)) |>
    select(Variable = variable, Term = term, sumsq_Alpine, df_Alpine, statistic_Alpine, p.value_Alpine, `sumsq_Sub-alpine`, `df_Sub-alpine`, `statistic_Sub-alpine`, `p.value_Sub-alpine`) |>
    group_by(Variable)



  climate |>
    mutate(Variable = factor(Variable, levels = c("air", "ground", "soil", "soilmoisture"))) |>
    arrange(Variable) |>
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

