# piecewiseSEM plan

piecewiseSEM_plan <- list(

  ### colours

  tar_target(
    name = sem_colour,
    command = wes_palette("FrenchDispatch")
  ),

  # CHANGE IN BIOMASS AND DIVERSITY (LOG RATIO)
  # CUTTING
  # prep data
  tar_target(
    name = cut_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "cutting",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)
  ),

  # run sem without site
  tar_target(
    name = cut_change,
    command = run_SEM(data = cut_change_data,
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_out,
    command = summary(cut_change)

  ),

  # figure
  tar_target(
    name = cut_change_fig,
    command = make_SEM_figure(sem_results = cut_change_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # run site sem
  tar_target(
    name = cut_change_site,
    command = run_site_SEM(data = cut_change_data,
                           landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_site_out,
    command = summary(cut_change_site)

  ),

  # figure
  tar_target(
    name = cut_change_site_fig,
    command = make_SEM_site_figure(sem_results = cut_change_site_out,
                                   type = "change",
                                   landuse = "cutting",
                                   col = sem_colour)
  ),

  # sem by origin
  # alpine
  tar_target(
    name = cut_change_alp,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Alpine"),
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_alp_out,
    command = summary(cut_change_alp)

  ),

  # figure
  tar_target(
    name = cut_change_alp_fig,
    command = make_SEM_figure(sem_results = cut_change_alp_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # sub-alpine
  tar_target(
    name = cut_change_sub,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "cutting")
  ),

  # summary
  tar_target(
    name = cut_change_sub_out,
    command = summary(cut_change_sub)

  ),

  # figure
  tar_target(
    name = cut_change_sub_fig,
    command = make_SEM_figure(sem_results = cut_change_sub_out,
                              type = "change",
                              landuse = "cutting",
                              col = sem_colour)
  ),

  # Figure 2
  tar_target(
    name = cut_change_figure,
    command = {

      (cut_change_fig + cut_change_site_fig) /
        (cut_change_alp_fig + cut_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_cut_stats,
    command = {

      bind_rows(
        Across = cut_change_out$coefficients,
        Site = cut_change_site_out$coefficients,
        Alpine = cut_change_alp_out$coefficients,
        "Sub-alpine" = cut_change_sub_out$coefficients,
        .id = "Type"
      )

    }
  ),


  # GRAZING
  # prep data
  tar_target(
    name = graz_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)
  ),

  # run sem
  tar_target(
    name = graz_change,
    command = run_SEM(data = graz_change_data,
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_out,
    command = summary(graz_change)

  ),

  # sem grazing figure
  tar_target(
    name = graz_change_fig,
    command = make_SEM_figure(sem_results = graz_change_out,
                                   type = "change",
                                   landuse = "grazing",
                              col = sem_colour)
  ),

  # run site sem
  tar_target(
    name = graz_change_site,
    command = run_site_SEM(data = graz_change_data,
                           landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_site_out,
    command = summary(graz_change_site)

  ),

  # figure
  tar_target(
    name = graz_change_site_fig,
    command = make_SEM_site_figure(sem_results = graz_change_site_out,
                                   type = "change",
                                   landuse = "grazing",
                                   col = sem_colour)
  ),

  # sem by origin
  # alpine
  tar_target(
    name = graz_change_alp,
    command = run_SEM(data = graz_change_data |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_alp_out,
    command = summary(graz_change_alp)

  ),

  # figure
  tar_target(
    name = graz_change_alp_fig,
    command = make_SEM_figure(sem_results = graz_change_alp_out,
                              type = "change",
                              landuse = "grazing",
                              col = sem_colour)
  ),

  # sub-alpine data
  tar_target(
    name = graz_change_sub,
    command = run_SEM(data = graz_change_data |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")
  ),

  # summary
  tar_target(
    name = graz_change_sub_out,
    command = summary(graz_change_sub)

  ),

  # figure
  tar_target(
    name = graz_change_sub_fig,
    command = make_SEM_figure(sem_results = graz_change_sub_out,
                              type = "change",
                              landuse = "grazing",
                              col = sem_colour)
  ),

  # Figure 2
  tar_target(
    name = graz_change_figure,
    command = {

      (graz_change_fig + graz_change_site_fig) /
        (graz_change_alp_fig + graz_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Across sites', 'b) Including site', 'c) Alpine site', 'd) Sub-alpine site'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_graz_stats,
    command = {

      bind_rows(
        Across = graz_change_out$coefficients,
        Site = graz_change_site_out$coefficients,
        Alpine = graz_change_alp_out$coefficients,
        "Sub-alpine" = graz_change_sub_out$coefficients,
        .id = "Type"
      )

    }
  ),

  tar_target(
    name = div_figure,
    command = make_sem_div(cut_change_data,
                           sem_colour)
  ),

  tar_target(
    name = rich_figure,
    command = make_sem_rich(biomass_div,
                           sem_colour)
  ),

  tar_target(
    name = env_figure,
    command = make_sem_even(biomass_div,
                           sem_colour)
  )

)
