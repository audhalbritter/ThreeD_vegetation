### Winners and losers

get_winners_and_losers <- function(cover_total){

  #first and last year transplant comm by treatment
  #get first_transplant (year 1)
  first_transplant = cover_total |>
    filter(year == 2019)  |>
    select(origSiteID, destSiteID, destBlockID, turfID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year, species, cover)

  # get last_transplant (last year)
  last_transplant = cover_total |>
    filter(year == 2022)  |>
    select(origSiteID, destSiteID, destBlockID, turfID, warming, Namount_kg_ha_y, Nitrogen_log, grazing, grazing_num, year, species, cover)

  #losers = first - last year
  losers = anti_join(first_transplant, last_transplant, by = c("origSiteID", "destSiteID", "destBlockID", "turfID", "warming", "Namount_kg_ha_y", "Nitrogen_log", "grazing", "grazing_num", "species"))

  #winners = last - first year
  winners = anti_join(last_transplant, first_transplant, by = c("origSiteID", "destSiteID", "destBlockID", "turfID", "warming", "Namount_kg_ha_y", "Nitrogen_log", "grazing", "grazing_num", "species"))

  # combine winner and losers
  winn_loos <- bind_rows(colonization = winners,
                          extinction = losers,
                         .id = "status") |>
    select(-cover)

  # join with cover data to get which species are winners and losers
  cover_all <- cover_total |>
    tidylog::left_join(winn_loos,
                       by = c("year", "origSiteID", "destSiteID", "destBlockID", "turfID", "warming", "Namount_kg_ha_y", "Nitrogen_log", "grazing", "grazing_num", "species"))

  # species that increase and decrease in cover or stay the same
  cov <- cover_all |>
    # remove winners and losers
    filter(is.na(status)) |>
    pivot_wider(names_from = year, values_from = cover) |>
    # change
    mutate(change = `2022` - `2019`,
           log_ratio = log(`2022` / `2019`),
           rel_change = (change / `2019`) * 100) |>
    # categorize species into not changing, increasing and decreasing in cover
    mutate(status2 = case_when(rel_change >= 10 ~ "increase",
                              rel_change <= -10 ~ "decrease",
                              TRUE ~ "stable")) |>
    select(-`2019`, -`2022`)

  cover_all |>
    tidylog::left_join(cov, by = c("origSiteID", "origBlockID", "origPlotID", "destSiteID", "destBlockID", "destPlotID", "turfID", "warming", "Namount_kg_ha_y", "Nitrogen_log", "Nlevel", "grazing", "grazing_num", "species", "status")) |>
    mutate(status = if_else(is.na(status), status2, status)) |>
    select(-status2) |> 
    # merge for winner and losers
    mutate(status2 = if_else(status %in% c("extinction", "decrease"), "loser", "winner"))

  ### PROBABLY WANT TO REMOVE YEAR AND INCREASE, DECREASE AND STABLE FROM 2019. THEY ARE THE SAME AS FROM 2022. QUESTION, DO WE NEED COVER FROM 2019, AND WANT TO KEEP THEM?

}



make_trait_impute2 <- function(winn_lose, trait_raw, ellenberg){

  #prepare community data
  comm <- winn_lose |>

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
                               TRUE ~ species))

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
    mutate(grazing_num = recode(grazing, Control = "0", Medium = "2", Intensive  = "4"),
           grazing_num = as.numeric(grazing_num)) |>

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
                             other_col = c("destSiteID", "warming", "grazing", "grazing_num",  "Namount_kg_ha_y", "Nitrogen_log"),
                             min_n_in_sample = 2)

  trait_impute

}

# trait_mean_wl %>%
#   autoplot(., other_col_how = "ignore") +
#   scale_fill_manual(labels = c("turfID", "blockID", "siteID", "global"),
#                     values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00")) +
#   scale_y_continuous(breaks = c(0, 0.5, 1)) +
#   facet_wrap(~ trait_trans, labeller = labeller(trait_trans = trait_names)) +
#   labs(x = "Treatments") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90))
