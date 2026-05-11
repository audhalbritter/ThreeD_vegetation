# figure plan

figure_plan <- list(

  ### Figure 3
  # standing biomass and drivers
  tar_target(
    name = bio_div_figure,
    command = {
      # variance explained labels
      biomass_r2 <- biomass_origin_output |>
        transmute(origSiteID,
          r2_label = paste0(
            "R² = ", round(r.squared, 2),
            "; Adj. R² = ", round(adj.r.squared, 2)
          )
        )

      diversity_r2 <- diversity_origin_output |>
        filter(diversity_index == "diversity") |>
        transmute(origSiteID,
          r2_label = paste0(
            "R² = ", round(r.squared, 2),
            "; Adj. R² = ", round(adj.r.squared, 2)
          )
        )

      ### Figure 3a Biomass by origin
      biomass_text2 <- biomass_origin_anova_table |>
        mutate(significance = case_when(
          term == "Residuals" ~ "non-sign",
          p.value >= 0.07 ~ "non-sign",
          p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
          TRUE ~ "sign"
        )) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))

      biomass_text3 <- biomass_origin_anova_table |>
        mutate(significance = case_when(
          term == "Residuals" ~ "non-sign",
          p.value >= 0.07 ~ "non-sign",
          p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
          TRUE ~ "sign"
        )) |>
        # BY HAND CODE!!!
        filter(significance %in% c("marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))


      bio <- make_vegetation_figure(
        dat1 = biomass_origin_output |>
          unnest(data),
        x_axis = Nitrogen_log,
        yaxislabel = bquote(Standing ~ biomass ~ g ~ m^-2),
        colourpalette = warming_palette,
        linetypepalette = c("solid", "dashed", "dotted"),
        shapepalette = c(21, 22, 24),
        facet_2 = NA,
        # predictions
        dat2 = biomass_origin_prediction
      ) +
        labs(tag = "a)") +
        # add stats
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(1),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 1.4, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(2),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 3, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(3),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.2, vjust = 4.6, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(4),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 6.2, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text3 |>
                group_by(origSiteID) |>
                slice(1),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 6.2, label = term),
          size = 3, colour = "grey50", nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(5),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 7.8, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = biomass_origin_prediction |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              biomass_text2 |>
                group_by(origSiteID) |>
                slice(6),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 9.4, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        # add R2 labels
        geom_text(
          data = biomass_r2,
          inherit.aes = FALSE,
          aes(x = Inf, y = Inf, label = r2_label, group = origSiteID),
          hjust = 1.05, vjust = 1.2,
          size = 3.5, colour = text_colour
        )

      ### Figure 3b DIVERSITY BY ORIGIN
      div_text2 <- diversity_origin_anova_table |>
        filter(diversity_index == "diversity") |>
        mutate(significance = case_when(
          term == "Residuals" ~ "non-sign",
          p.value >= 0.07 ~ "non-sign",
          p.value >= 0.05 & p.value <= 0.07 ~ "marginal",
          TRUE ~ "sign"
        )) |>
        # BY HAND CODE!!!
        filter(significance %in% c("sign", "marginal")) |>
        distinct(origSiteID, term, significance) |>
        mutate(term = factor(term, levels = c("W", "N", "C", "S", "WxN", "WxC", "NxC", "WxNxC")))


      div <- make_vegetation_figure(
        dat1 = diversity_origin_output |>
          filter(diversity_index == "diversity") |>
          unnest(data),
        x_axis = Nitrogen_log,
        yaxislabel = "Shannon diversity",
        colourpalette = warming_palette,
        linetypepalette = c("solid", "dashed", "dotted"),
        shapepalette = c(21, 22, 24),
        facet_2 = NA,
        # predictions
        dat2 = diversity_origin_prediction |>
          filter(diversity_index == "diversity")
      ) +
        labs(tag = "b)") +
        # add stats
        geom_text(
          data = diversity_origin_prediction |>
            filter(diversity_index == "diversity") |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              div_text2 |>
                group_by(origSiteID) |>
                slice(1),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = -Inf, hjust = -0.2, vjust = -1.4, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = diversity_origin_prediction |>
            filter(diversity_index == "diversity") |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              div_text2 |>
                group_by(origSiteID) |>
                slice(2),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = -Inf, hjust = -0.3, vjust = -3, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        geom_text(
          data = diversity_origin_prediction |>
            filter(diversity_index == "diversity") |>
            distinct(origSiteID, warming, Nitrogen_log, grazing) |>
            left_join(
              div_text2 |>
                group_by(origSiteID) |>
                slice(3),
              by = c("origSiteID")
            ),
          aes(x = -Inf, y = -Inf, hjust = -0.05, vjust = -4.6, label = term),
          size = 3, colour = text_colour, nudge_x = 50
        ) +
        # add R2 labels
        geom_text(
          data = diversity_r2,
          inherit.aes = FALSE,
          aes(x = Inf, y = Inf, label = r2_label, group = origSiteID),
          hjust = 1.05, vjust = 1.2,
          size = 3.5, colour = text_colour
        )

      (bio + div) + plot_layout(guides = "collect") &
        theme(
          legend.position = "top",
          plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
          legend.background = element_rect(fill = "transparent")
        )
    }
  ),
  tar_target(
    name = standingB_div_final_figure,
    command = {
      biomass_div |>
        filter(grazing != "Natural") |>
        ggplot(aes(x = log(final_bio), y = final_diversity)) +
        geom_point(
          aes(
            colour = warming,
            fill = warming,
            shape = grazing,
            stroke = 0.8,
            # size = #Namount_kg_ha_y),
            size = Nitrogen_log
          ),
          alpha = 0.5
        ) +
        scale_colour_manual(values = warming_palette, name = "Warming") +
        scale_fill_manual(values = warming_palette, name = "Warming") +
        scale_shape_manual(values = c(21, 22, 24), name = "Clipping") +
        scale_size_continuous(
          name = bquote(Log(Nitrogen) ~ kg ~ ha^-1 ~ y^-1),
          breaks = c(0, 1, 2, 3, 4),
          labels = c("0", "25", "50", "75", "100")
        ) +
        scale_linetype_manual(
          values = c("solid", "dashed"),
          name = "Origin"
        ) +
        labs(
          x = bquote(Log(Standing ~ biomass) ~ g ~ m^-2),
          y = "Shannon diversity"
        ) +
        facet_wrap(vars(origSiteID)) +
        theme_bw() +
        theme(
          legend.position = "top",
          legend.box = "vertical",
          text = element_text(size = 13)
        )
    }
  )

)
