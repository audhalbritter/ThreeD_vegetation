# si SEM plan

si_SEM_change_plan <- list(

# CHANGE IN BIOMASS AND DIVERSITY (LOG RATIO)
  # CUTTING
  # prep data
  tar_target(
    name = cut_change_data,
    command = prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_diversity,
                            biomass = log_ratio_bio)
  ),

  # sem by origin
  # alpine
  tar_target(
    name = cut_change_alp,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")
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
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")
  ),

  # sub-alpine
  tar_target(
    name = cut_change_sub,
    command = run_SEM(data = cut_change_data |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")
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
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")
  ),

  # Figure 2
  tar_target(
    name = cut_change_figure,
    command = {

        (cut_change_alp_fig + cut_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_cut_stats,
    command = {

      bind_rows(
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
                              col = treatment_palette,
                              diversity_type = "diversity")
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
                              col = treatment_palette,
                              diversity_type = "diversity")
  ),

  # Figure 2
  tar_target(
    name = graz_change_figure,
    command = {

        (graz_change_alp_fig + graz_change_sub_fig) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

    }
  ),

  tar_target(
    name = sem_graz_stats,
    command = {

      bind_rows(
        Alpine = graz_change_alp_out$coefficients,
        "Sub-alpine" = graz_change_sub_out$coefficients,
        .id = "Type"
      )

    }
  ),

  
# SEM other diversity indices
  # change in biomass and diversity indices
  # clipping and richness
  tar_target(
    name = cut_change_richness,
    command = {

      # change in richness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_richness,
                            biomass = log_ratio_bio)

      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "richness")

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  # clipping and evenness
  tar_target(
    name = cut_change_evenness,
    command = {

      # change in evenness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = log_ratio_evenness,
                            biomass = log_ratio_bio)

      # change in evenness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # change in evenness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "evenness")

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  # grazing and richness
  tar_target(
    name = graz_change_richness,
    command = {

      # change in richness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_richness,
                            biomass = log_ratio_bio)

      # change in richness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      # change in richness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "richness")

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  # grazing and evenness
  tar_target(
    name = graz_change_evenness,
    command = {

      # change in evenness
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "grazing",
                            diversity = log_ratio_evenness,
                            biomass = log_ratio_bio)

      # change in evenness alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "grazing")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      # change in evenness sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "grazing")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "change",
                              landuse = "grazing",
                              col = treatment_palette,
                              diversity_type = "evenness")

      figure <- (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine', 'b) Sub-alpine'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        Alpine = out3$coefficients,
        "Sub-alpine" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
