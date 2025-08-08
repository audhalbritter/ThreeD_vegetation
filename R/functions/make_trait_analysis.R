# trait bootstrapping functions

make_trait_impute <- function(cover_total, trait_raw, ellenberg){

  #prepare community data
  comm <- cover_total |> 
    filter(year == 2022) |>

    # make new variable for 3 treatments (AC0 is the control)
    mutate(warming2 = if_else(warming == "Ambient", "A", "W"),
           grazing2 = if_else(grazing == "Control", "C", "N"),
           nitrogen = case_when(Namount_kg_ha_y == 0.5 ~ 0,
                                Namount_kg_ha_y == 1 ~ 0,
                                Namount_kg_ha_y == 100 ~ 50,
                                TRUE ~ Namount_kg_ha_y),
           treatment = paste0(warming2, grazing2, nitrogen),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AN0", "WN0",
                                                    "AN5", "WN5",
                                                    "AN10", "WN10",
                                                    "AN50", "WN50")),

           #grazing = factor(grazing, levels = c("Control", "Natural")),

           # make same as traits
           siteID = recode(destSiteID, "Lia" = "Liahovden", "Joa" = "Joasete", "Vik" = "Vikesland"),
           blockID = destBlockID) |>
    # fix species names so it matches ellenberg values
    mutate(species = str_replace(species, " cf", ""),
           species = case_when(species == "Betula pubescence" ~ "Betula pubescens",
                               species == "Salix herbaceae" ~ "Salix herbacea",
                               species == "Trientalis europea" ~ "Trientalis europaea",
                               species == "Oxytropa laponica" ~ "Oxytropis lapponica",
                               TRUE ~ species)) |> 
    # make grazing numeric
    mutate(grazing_num = case_when(grazing == "Control" ~ 0,
                                   grazing == "Medium" ~ 2,
                                   grazing == "Intensive" ~ 4,
                                   grazing == "Natural" ~ 2))

  #prepare trait data
  trait <- trait_raw |> 
    tidylog::filter(siteID != "Hogsete",
                    !(siteID == "Vikesland" & gradient == "gradient") | is.na(gradient)) |>
    select(-gradient) |>

    # remove missing treatment (3 leaves)
    tidylog::filter(!is.na(grazing)) |>

    # remove wet mass
    filter(trait != "wet_mass_g") |>

    # merge species like for cover data
    mutate(species = case_when(str_detect(species, "Antennaria") ~ "Antennaria sp",
                               str_detect(species, "Pyrola") ~ "Pyrola sp",
                               species == "Betula pubescence" ~ "Betula pubescens",
                               species == "Salix herbaceae" ~ "Salix herbacea",
                               species == "Trientalis europea" ~ "Trientalis europaea",
                               species == "Oxytropa laponica" ~ "Oxytropis lapponica",
                               TRUE ~ species)) |>
    # log transform size traits
    mutate(
      value_trans = if_else(
        trait %in% c(
          "plant_height_cm",
          "dry_mass_g",
          "leaf_area_cm2",
          "leaf_thickness_mm"
        ),
        true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
        false = value
      ),
      trait_trans = recode(
        trait,
        "plant_height_cm" = "plant_height_cm_log",
        "dry_mass_g" = "dry_mass_g_log",
        "leaf_area_cm2" = "leaf_area_cm2_log",
        "leaf_thickness_mm" = "leaf_thickness_mm_log"
      ),
      trait_trans = factor(trait_trans, levels = c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "leaf_thickness_mm_log", "sla_cm2_g", "ldmc"))) |>

    # add origSiteID for 23 leaves where it is missing (have no turfID)
    tidylog::mutate(origSiteID = if_else(is.na(origSiteID), "Joasete", origSiteID),
                    destSiteID = if_else(is.na(destSiteID), "Joasete", destSiteID)) |>

    # log transform Nitrogen
    mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),

           # make new variable for 3 treatments (AC0 is the control)
           grazing2 = if_else(grazing == "Control", "C", "N"),
           nitrogen = case_when(Namount_kg_ha_y == 0.5 ~ 0,
                                Namount_kg_ha_y == 1 ~ 0,
                                Namount_kg_ha_y == 100 ~ 50,
                                TRUE ~ Namount_kg_ha_y),
           treatment = paste0(warming, grazing2, nitrogen),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                           "AC5", "WC5",
                                                           "AC10", "WC10",
                                                           "AC50", "WC50",
                                                           "AN0", "WN0",
                                                           "AN5", "WN5",
                                                           "AN10", "WN10",
                                                           "AN50", "WN50")),

           grazing = recode(grazing, "C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural"),
           grazing = factor(grazing, levels = c("Control", "Medium", "Intensive", "Natural")),
           warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
           Namount_kg_ha_y = as.character(Namount_kg_ha_y),
           blockID = as.numeric(blockID)) |>

    # make grazing numeric
    mutate(grazing_num = case_when(grazing == "Control" ~ 0,
                                   grazing == "Medium" ~ 2,
                                   grazing == "Intensive" ~ 4,
                                   grazing == "Natural" ~ 2)) |>
    # mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
    #        grazing_num = as.numeric(grazing_num)) |>

    # remove 27 accidental some observations with warm, grazing and N5
    filter(!is.na(treatment)) |>

    # add ellenberg
    pivot_wider(names_from = trait_trans, values_from = value_trans) |>
    # add ellenberg values
    tidylog::left_join(ellenberg, by = "species") |>
    pivot_longer(cols = c(plant_height_cm_log:sla_cm2_g, light:salinity),
                 names_to = "trait_trans",
                 values_to = "value_trans") |>

    # remove NAs from data
    filter(!is.na(value_trans)) |>

    select(siteID, blockID, turfID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log, treatment, species, trait_trans, value_trans, origSiteID, destSiteID)

  #set seed for bootstrapping repeatability
  set.seed(2525)
  trait_impute <- trait_fill(comm = comm,
                             traits = trait,
                             scale_hierarchy = c("origSiteID", "blockID", "turfID"),
                             taxon_col = "species",
                             trait_col = "trait_trans",
                             value_col = "value_trans",
                             abundance_col = "cover",
                             treatment_col = c("treatment"),
                             treatment_level = c("origSiteID"),
                             other_col = c("destSiteID", "warming", "grazing", "grazing_num",  "Namount_kg_ha_y", "Nitrogen_log", "Nlevel"),
                             min_n_in_sample = 2)

  trait_impute

}

#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){

  dat %>%
    mutate(trait_fancy = case_match(trait_trans,
                                    "plant_height_cm_log" ~ "Height cm",
                                    "dry_mass_g_log" ~ "Dry mass g",
                                    "leaf_area_cm2_log" ~ "Area cm2",
                                    "leaf_thickness_mm_log" ~ "Thickness mm",
                                    "ldmc" ~ "LDMC g/g",
                                    "sla_cm2_g" ~ "SLA cm2/g",
                                    "light" ~ "Light",
                                    "temperature" ~ "Temperature",
                                    "nutrients" ~ "Nutrients",
                                    "moisture" ~ "Moisture",
                                    "salinity" ~ "Salinity",
                                    "reaction" ~ "Reaction")) |>
    mutate(figure_names = case_match(trait_trans,
                                     "plant_height_cm_log" ~ "Plant~height~(cm)",
                                     "dry_mass_g_log" ~ "Leaf~dry~mass~(g)",
                                     "leaf_area_cm2_log" ~ "Leaf~area~(cm^2)",
                                     "leaf_thickness_mm_log" ~ "Leaf~thickness~(mm)",
                                     "ldmc" ~ "LDMC~(gg^{-1})",
                                     "sla_cm2_g" ~ "SLA~(cm^2*g^{-1})",
                                     "light" ~ "Light",
                                     "temperature" ~ "Temperature",
                                     "nutrients" ~ "Nutrients",
                                     "moisture" ~ "Moisture",
                                     "salinity" ~ "Salinity",
                                     "reaction" ~ "Reaction")) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)",
                                            "Leaf~dry~mass~(g)",
                                            "Leaf~area~(cm^2)",
                                            "Leaf~thickness~(mm)",
                                            "SLA~(cm^2*g^{-1})",
                                            "LDMC~(gg^{-1})",
                                            "Light",
                                            "Temperature",
                                            "Nutrients",
                                            "Moisture",
                                            "Salinity",
                                            "Reaction")))

}


#do the bootstrapping
make_bootstrapping <- function(trait_impute){

  CWM <- trait_np_bootstrap(trait_impute, nrep = 100, sample_size = 200)

  trait_mean <- trait_summarise_boot_moments(CWM) |>
    ungroup() |>
    select(-global, -n) |>
    fancy_trait_name_dictionary()

  trait_mean

}


# Test treatment effects on traits
test_treatment_effects <- function(data, biomass_data, 
traits = c("plant_height_cm_log", "temperature", "light", "moisture", "nutrients", "reaction")) {
  
  # Filter data for specified traits
  trait_data <- data |>
    filter(grazing != "Natural") |>
    filter(trait_trans %in% traits)
  
  # Test warming effects for each trait
  results_warming <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        # Ensure warming is a factor
        dat <- .x |> mutate(warming = as.factor(warming))
        lm(mean ~ warming, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test nitrogen effects for each trait
  results_nitrogen <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ Nitrogen_log, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test grazing effects for each trait (excluding Natural)
  results_grazing <- trait_data |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ grazing_num, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Test biomass effects for each trait (excluding Natural)
  results_biomass <- trait_data |>
    tidylog::left_join(
      biomass_data |> 
        filter(year == 2022, grazing != "Natural") |>
        mutate(biomass_log = log(standing_biomass)) |>
        select(-year),
      by = c("origSiteID", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel")
    ) |>
    group_by(trait_trans, trait_fancy, origSiteID, figure_names) |>
    nest() |>
    mutate(
      # Run linear model
      model = map(data, ~ {
        lm(mean ~ biomass_log, data = .x)
      }),
      
      # Get tidy results
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy)
    )

  # Combine results
  results <- bind_rows(
    warming = results_warming, 
    nitrogen = results_nitrogen,
    grazing = results_grazing,
    biomass = results_biomass,
    .id = "treatment"
  ) |>
    select(-data, -model, -anova)

  return(results)
}