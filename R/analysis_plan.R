# analysis plan

analysis_plan <- list(

  # Biomass vs diversity analysis
  tar_target(
    name = standingB_div_final_model,
    command = lm(final_diversity ~ log(final_bio) * origSiteID, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_final_prediction,
    command = augment(standingB_div_final_model)
  ),

  tar_target(
    name = standingB_div_change_model,
    command = lm(log_ratio_diversity ~ log_ratio_bio * origSiteID, data = biomass_div)
  ),

  tar_target(
    name = standingB_div_change_prediction,
    command = augment(standingB_div_change_model)
  ),

  # winners and loosers
  tar_target(
    name = winn_loos,
    command = get_winners_and_loosers(cover_total)
  )

)
