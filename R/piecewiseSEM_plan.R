# piecewiseSEM plan

piecewiseSEM_plan <- list(

  ### colours

  # Final biomass and diversity indices
  # clipping and diversity
  tar_target(
    name = cut_final_diversity,
    command = {

      # final diversity across site
      dat1 <- prep_SEM_data(data = biomass_div,
                            landuse = "clipping",
                            diversity = final_diversity,
                            biomass = final_bio)

      # final diversity alpine
      mod3 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")

      # final diversity sub-alpine
      mod4 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "diversity")

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

  tar_target(
    name = cut_affinity,
    command = {

      # affinity data
      dat <- biomass_div |> 
        filter(grazing != "Natural") |>
        tidylog::left_join(bind_rows(
        affinity_pca[[1]]
      ), by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log))

      # prep data
      dat1 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC1,
                            biomass = final_bio)

      # affinity alpine
      mod1 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # affinity sub-alpine
      mod2 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out2 <- summary(mod2)

      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # PC2 analyses
      # prep data for PC2
      dat2 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC2,
                            biomass = final_bio)

      # PC2 affinity alpine
      mod3 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # PC2 affinity sub-alpine
      mod4 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      figure <- (fig1 + fig2) / (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine PC1', 'b) Sub-alpine PC1', 'c) Alpine PC2', 'd) Sub-alpine PC2'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        "Affinity: Alpine PC1" = out1$coefficients,
        "Affinity: Sub-alpine PC1" = out2$coefficients,
        "Affinity: Alpine PC2" = out3$coefficients,
        "Affinity: Sub-alpine PC2" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  tar_target(
    name = cut_traits,
    command = {

      # trait data
      dat <- biomass_div |> 
        filter(grazing != "Natural") |>
        tidylog::left_join(bind_rows(
        trait_pca[[1]]
      ), by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log))

      # prep data
      dat1 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC1,
                            biomass = final_bio)

      # trait alpine
      mod1 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits")

      # trait sub-alpine
      mod2 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out2 <- summary(mod2)

      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits")

      # PC2 analyses
      # prep data for PC2
      dat2 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC2,
                            biomass = final_bio)

      # PC2 trait alpine
      mod3 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits")

      # PC2 trait sub-alpine
      mod4 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits")

      figure <- (fig1 + fig2) / (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine PC1', 'b) Sub-alpine PC1', 'c) Alpine PC2', 'd) Sub-alpine PC2'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        "Traits: Alpine PC1" = out1$coefficients,
        "Traits: Sub-alpine PC1" = out2$coefficients,
        "Traits: Alpine PC2" = out3$coefficients,
        "Traits: Sub-alpine PC2" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),

  tar_target(
    name = cut_affinity2,
    command = {

      # affinity data
      dat <- biomass_div |> 
        filter(grazing != "Natural") |>
        tidylog::left_join(bind_rows(
        affinity_alpine_pca[[1]],
        affinity_subalpine_pca[[1]]
      ), by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log))

      # prep data
      dat1 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC1,
                            biomass = final_bio)

      # traits alpine
      mod1 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # traits sub-alpine
      mod2 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out2 <- summary(mod2)

      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # PC2 analyses
      # prep data for PC2
      dat2 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC2,
                            biomass = final_bio)

      # PC2 traits alpine
      mod3 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      # PC2 traits sub-alpine
      mod4 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "affinity")

      figure <- (fig1 + fig2) / (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine PC1', 'b) Sub-alpine PC1', 'c) Alpine PC2', 'd) Sub-alpine PC2'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        "Affinity: Alpine PC1" = out1$coefficients,
        "Affinity: Sub-alpine PC1" = out2$coefficients,
        "Affinity: Alpine PC2" = out3$coefficients,
        "Affinity: Sub-alpine PC2" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  ),


  tar_target(
    name = cut_traits2,
    command = {

      # traits data
      dat <- biomass_div |> 
        filter(grazing != "Natural") |>
        tidylog::left_join(bind_rows(
        trait_alpine_pca[[1]],
        trait_subalpine_pca[[1]]
      ), by = join_by(origSiteID, warming, grazing, grazing_num, Nlevel, Namount_kg_ha_y, Nitrogen_log))

      # prep data
      dat1 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC1,
                            biomass = final_bio)

      # traits alpine
      mod1 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out1 <- summary(mod1)

      fig1 <- make_SEM_figure(sem_results = out1,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits PC1")

      # traits sub-alpine
      mod2 <- run_SEM(data = dat1 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out2 <- summary(mod2)

      fig2 <- make_SEM_figure(sem_results = out2,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits PC1")

      # PC2 analyses
      # prep data for PC2
      dat2 <- prep_SEM_data(data = dat,
                            landuse = "clipping",
                            diversity = PC2,
                            biomass = final_bio)

      # PC2 traits alpine
      mod3 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Alpine"),
                      landuse = "clipping")

      out3 <- summary(mod3)

      fig3 <- make_SEM_figure(sem_results = out3,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits PC2")

      # PC2 traits sub-alpine
      mod4 <- run_SEM(data = dat2 |>
                        filter(origSiteID == "Sub-alpine"),
                      landuse = "clipping")

      out4 <- summary(mod4)

      fig4 <- make_SEM_figure(sem_results = out4,
                              type = "final",
                              landuse = "clipping",
                              col = treatment_palette,
                              diversity_type = "traits PC2")

      figure <- (fig1 + fig2) / (fig3 + fig4) +
        plot_annotation(tag_levels = list(c('a) Alpine PC1', 'b) Sub-alpine PC1', 'c) Alpine PC2', 'd) Sub-alpine PC2'))) &
        theme(plot.tag.position = c(0, 1),
              plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

      out <- bind_rows(
        "Traits: Alpine PC1" = out1$coefficients,
        "Traits: Sub-alpine PC1" = out2$coefficients,
        "Traits: Alpine PC2" = out3$coefficients,
        "Traits: Sub-alpine PC2" = out4$coefficients,
        .id = "Type"
      )

      outputList <- list(figure, out)

    }
  )

)
