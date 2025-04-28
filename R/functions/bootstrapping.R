# trait bootstrapping functions

make_trait_impute <- function(cover_total, trait_raw, ellenberg){

  #prepare community data
  comm <- cover_total |>
    ungroup() |>
    filter(year == 2022,
           grazing %in% c("Control", "Natural"),
           Nlevel %in% c(1, 2, 3, 6, 7, 8)) |>

    # fix species names so it matches ellenberg values
    mutate(species = str_replace(species, " cf", ""),
           species = case_when(species == "Betula pubescence" ~ "Betula pubescens",
                               species == "Salix herbaceae" ~ "Salix herbacea",
                               species == "Trientalis europea" ~ "Trientalis europaea",
                               species == "Oxytropa laponica" ~ "Oxytropis lapponica",
                               TRUE ~ species)) |>

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),

           # make new variable for 3 treatments (AC0 is the control)
           warming2 = if_else(warming == "Ambient", "A", "W"),
           grazing2 = if_else(grazing == "Control", "C", "N"),
           treatment = paste0(warming2, grazing2, Namount_kg_ha_y),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AN0", "WN0")),

           grazing = factor(grazing, levels = c("Control", "Natural")),

           # make same as traits
           siteID = recode(destSiteID, "Lia" = "Liahovden", "Joa" = "Joasete", "Vik" = "Vikesland"),
           blockID = destBlockID) |>

    # remove treatments with no trait data
    tidylog::filter(!is.na(treatment))



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
                               TRUE ~ species)) |>
    # fix species names to match ellenberg values
    mutate(species = str_replace(species, " cf", ""),
           species = case_when(species == "Betula pubescence" ~ "Betula pubescens",
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

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
           #origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),

           # make new variable for 3 treatments (AC0 is the control)
           treatment = paste0(warming, grazing, Namount_kg_ha_y),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AN0", "WN0")),

           grazing = recode(grazing, "C" = "Control", "N" = "Natural"),
           grazing = factor(grazing, levels = c("Control", "Natural")),

           warming = recode(warming, "A" = "Ambient", "W" = "Warming"),

           Namount_kg_ha_y = as.character(Namount_kg_ha_y),
           #Namount_kg_ha_y = factor(Namount_kg_ha_y, levels = c("0", "5", "10", "50", "150")

           blockID = as.numeric(blockID)) |>
    # remove 27 accidental some observations with warm, grazing and N5
    filter(!is.na(treatment)) |>

    pivot_wider(names_from = trait, values_from = value) |>
    # add ellenberg values
    tidylog::left_join(ellenberg, by = "species") |>
    pivot_longer(cols = c(plant_height_cm:sla_cm2_g, light:salinity),
                 names_to = "trait",
                 values_to = "value") |>

    select(siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, treatment, species, trait_trans, value_trans, origSiteID, destSiteID)

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
                          other_col = c("destSiteID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log"),
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
