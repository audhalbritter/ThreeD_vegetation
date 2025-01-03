# piecewiseSEM plan

piecewiseSEM_plan <- list(

  # SEM cutting and diversity
  tar_target(
    name = prep_cutting_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = log_ratio_diversity)
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
    command = make_SEM_figure(sem_results = sem_cutting_result,
                              landuse = "cutting")
  ),


  # SEM grazing and diversity
  tar_target(
    name = prep_grazing_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_diversity)
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
    command = make_SEM_figure(sem_results = sem_grazing_result,
                              landuse = "grazing")
  ),

  tar_target(
    name = sem_fig,
    command = sem_cutting_fig + sem_grazing_fig + plot_annotation(tag_levels = list(c("a) Cutting", "b) Grazing")))
  )

)
