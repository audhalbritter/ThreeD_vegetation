# NMDS ordination

library(glue)

tar_target(
  name = NMDS_ordination,
  command = {
    cover_wide <- cover |>
      filter(!is.na(cover)) |>
      #select(-cover, -functional_group) |>
      mutate(presence = 1) |>
      # mutate(cover = sqrt(cover)) |>
      pivot_wider(names_from = species,
                  values_from = presence,
                  values_fill = 0) |>
      ungroup()

    sp_wide <- cover_wide |>
      select(-c(year:functional_group))

    res <- metaMDS(sp_wide, noshare = TRUE, try = 30, k = 2)

    NMDS_1 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 1)
    NMDS_2 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 2)
    NMDS_3 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 3)
    NMDS_4 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 4)
    NMDS_5 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 5)
    NMDS_6 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 6)

    stress_plot <- tibble(
      stress = c(NMDS_1$stress, NMDS_2$stress, NMDS_3$stress, NMDS_4$stress, NMDS_5$stress, NMDS_6$stress),
      dimensions = c(1:6)) %>%
      ggplot(aes(x = dimensions, y = stress)) +
      geom_point()




    cover_wide <- cover |>
      filter(!is.na(cover),
             grazing != "Natural",
             Namount_kg_ha_y == 0,
             grazing == "Control") |>
      select(-functional_group) |>
      mutate(cover = sqrt(cover)) |>
      pivot_wider(names_from = species,
                  values_from = cover,
                  values_fill = 0) |>
      ungroup()

    sp_wide <- cover_wide |>
      select(-c(year:grazing_num))

    res <- rda(sp_wide)

    out <- bind_cols(cover_wide |>
                       select(year:grazing_num),
                     fortify(res) |>
                       filter(Score == "sites"))

    sp <- fortify(res) |>
      filter(Score == "species")

    e_B <- eigenvals(res)/sum(eigenvals(res))

    species <- sp |>
      mutate(length = sqrt(PC1^2 + PC2^2)) |>
      filter(length > 0.7) |>
      select(Label, length)


    out |>
      mutate(ID = paste(warming, grazing, Namount_kg_ha_y)) |>
      #filter(origSiteID == "Alpine") |>
      ggplot(aes(x = PC1, y = PC2, colour = origSiteID, shape = warming)) +
      ## arrows
      # geom_segment(data = species, aes(x = 0, y = 0, xend = PC1, yend = PC2),
      #              arrow=arrow(length=unit(0.2,"cm")),
      #              alpha = 0.75, color = 'grey70') +
      # points and path
      geom_point(aes(size = ifelse(year == min(as.numeric(year)), "First", "Other"))) +
      geom_path(aes(group = ID, linetype = warming, colour = origSiteID)) +
      coord_equal() +
      #stat_ellipse(aes(colour = Namount_kg_ha_y)) +
      labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
           y = glue("PCA1 ({round(e_B[2] * 100, 1)}%)")) +

      #scale_colour_manual(values = c("grey", "#CC79A7")) +
      #scale_colour_viridis_c(direction = -1, begin = 0.1, end = 0.95) +
      scale_size_discrete(name = "Year", range = c(1.5, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
      scale_linetype_manual(values = c("dashed", "solid")) +
      scale_shape_manual(values = c(1, 17)) +
      #facet_wrap(~ grazing) +

      theme_bw() +
      theme(legend.box="vertical",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 17))

    ggsave(filename = "output/pca.png", fig, dpi = 300, width = 12, height = 8)



    # NMDS_1 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 1)
    # NMDS_2 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 2)
    # NMDS_3 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 3)
    # NMDS_4 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 4)
    # NMDS_5 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 5)
    # NMDS_6 <-  metaMDS(sp_wide, noshare = TRUE, try = 30, k = 6)
    #
    # stress_plot <- tibble(
    #   stress = c(NMDS_1$stress, NMDS_2$stress, NMDS_3$stress, NMDS_4$stress, NMDS_5$stress, NMDS_6$stress),
    #   dimensions = c(1:6)) %>%
    #   ggplot(aes(x = dimensions, y = stress)) +
    #   geom_point()

    return(stress_plot)

  })


