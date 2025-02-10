# piecewiseSEM plan

piecewiseSEM_plan <- list(

  # SEM final year biomass and diversity
  # cutting
  tar_target(
    name = prep_cutting_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = final_diversity,
                            biomass = final_bio,
                            change = FALSE)
  ),

  # run sem for drivers, biomass (calculated) and diversity, only cutting
  tar_target(
    name = sem_cutting,
    command = run_SEM(data = prep_cutting_data,
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = sem_cutting_result,
    command = summary(sem_cutting)

  ),

  # sem cutting figure
  tar_target(
    name = sem_cutting_fig,
    command = make_SEM_site_figure(sem_results = sem_cutting_result,
                              landuse = "cutting")
  ),

  # only direct effects (final)
  tar_target(
    name = direct_sem_cutting,
    command = run_direct_SEM(data = prep_cutting_data,
                             landuse = "cutting",
                             diversity = final_diversity,
                             biomass = final_ratio_bio)
  ),

  # SEM change in biomass and diversity
  # cutting
  tar_target(
    name = prep_cutting_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio,
                            change = FALSE)
  ),

  # run sem for drivers, biomass (calculated) and diversity, only cutting
  tar_target(
    name = sem_cutting_change,
    command = run_SEM(data = prep_cutting_change_data,
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = sem_cutting_change_result,
    command = summary(sem_cutting_change)

  ),

  # sem cutting figure
  tar_target(
    name = sem_cutting_change_fig,
    command = make_SEM_change_site_figure(sem_results = sem_cutting_change_result,
                              landuse = "cutting")
  ),

  # SEM final biomass and diversity
  # grazing
  tar_target(
    name = prep_grazing_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = final_diversity,
                            biomass = final_bio,
                            change = FALSE)
  ),

  # run sem for drivers, biomass (calculated) and diversity, only grazing
  tar_target(
    name = sem_grazing,
    command = run_SEM(data = prep_grazing_data,
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = sem_grazing_result,
    command = summary(sem_grazing)

  ),

  # sem grazing figure
  tar_target(
    name = sem_grazing_fig,
    command = make_SEM_site_figure(sem_results = sem_grazing_result,
                              landuse = "grazing")
  ),

  # only direct effects
  tar_target(
    name = direct_sem_grazing,
    command = run_direct_SEM(data = prep_grazing_data,
                             landuse = "grazing",
                             diversity = final_diversity,
                             biomass = final_bio)
  ),

  # SEM change in biomass and diversity
  # grazing
  tar_target(
    name = prep_grazing_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio,
                            change = FALSE)
  ),

  # run sem for drivers, biomass (calculated) and diversity, only cutting
  tar_target(
    name = sem_grazing_change,
    command = run_SEM(data = prep_grazing_change_data,
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = sem_grazing_change_result,
    command = summary(sem_grazing_change)

  ),

  # sem cutting figure
  tar_target(
    name = sem_grazing_change_fig,
    command = make_SEM_change_site_figure(sem_results = sem_grazing_change_result,
                              landuse = "grazing")
  ),

  tar_target(
    name = sem_final_fig,
    command = sem_cutting_fig + sem_grazing_fig + plot_annotation(tag_levels = list(c("a) Cutting", "b) Grazing")))
  ),

  tar_target(
    name = sem_change_fig,
    command = sem_cutting_change_fig + sem_grazing_change_fig + plot_annotation(tag_levels = list(c("a) Cutting", "b) Grazing")))
  )

)
