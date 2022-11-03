### make vegetation figures
### biomass and productivity

make_functional_group_biomass_figure <- function(biomass){

  biomass_fg_figure <- biomass |>
    # filter only 2022
    filter(year == 2022,
           (grazing == "Medium" & cut == 2) |
           (grazing %in% c("Intensive", "Control") & cut == 3)) |>
    mutate(warm_site = paste(origSiteID, warming, sep = " "),
           fun_group = factor(fun_group, levels = c("shrub", "graminoids", "cyperaceae", "forbs", "legumes", "bryophytes", "litter"))) |>
    ggplot(aes(x = factor(Namount_kg_ha_y), y = biomass_scaled, fill = fun_group)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = c("darkgreen", "limegreen", "lawngreen", "plum4", "plum2", "orange", "peru"), name = "") +
    labs(y = "Proportional functional group composition",
         x = bquote(Nitrogen~(kg~ha^-1~y^-1))) +
    facet_grid(warm_site ~ grazing) +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 12))

  return(biomass_fg_figure)


}

make_productivity_figure <- function(biomass){

  productivity_figure <- biomass |>
    filter(grazing != "Natural",
           !fun_group %in% c("litter")) %>%
    # summarise the cuts to get annual productivity
    group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year) %>%
    summarise(annual_productivity = sum(biomass_scaled)) %>%
    ggplot(aes(x = Nitrogen_log, y = annual_productivity, shape = factor(year), linetype = factor(year), colour = warming)) +
    geom_point() +
    geom_smooth(method = "lm", aes(fill = warming), alpha = 0.2) +
    scale_colour_manual(values = c("grey", "#CC79A7"), name = "") +
    scale_fill_manual(values = c("grey", "#CC79A7"), name = "") +
    scale_shape_manual(values = c(16, 1), name = "") +
    scale_linetype_manual(values = c("solid", "dashed"), name = "") +
    labs(y = bquote(Productivity~(g~m^-2~y^-1)),
         x = bquote(log(Nitrogen~kg~ha^-1~y^-1))) +
    facet_grid(origSiteID ~ grazing, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 12))

  # productivity_figure <- biomass |>
  #   filter(#grazing != "Natural",
  #          !fun_group %in% c("litter")) %>%
  #   # summarise the cuts to get annual productivity
  #   group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year) %>%
  #   summarise(annual_productivity = sum(biomass_scaled)) %>%
  #   filter(annual_productivity < 700,
  #          warming == "Ambient",
  #          Namount_kg_ha_y == 0) |>
  #   ggplot(aes(x = factor(year), y = annual_productivity, fill = grazing)) +
  #   geom_boxplot() +
  #   labs(y = bquote(Productivity~(g~m^-2~y^-1)),
  #        x = "") +
  #   scale_fill_viridis_d() +
  #   facet_grid(origSiteID ~ grazing, scales = "free_y") +
  #   theme_minimal() +
  #   theme(legend.position = "none")

  return(productivity_figure)

}


# 3 colour palette (grey, orange, pink)
#three_col_palette <- c("#999999", "#E69F00", "#CC79A7")

#        sign = if_else(is.na(sign),  "no effect", sign),
#        sign = recode(sign, W_sign = "Warming", NxW_sign = "Interaction"),
#        sign = factor(sign, levels = c("no effect", "Warming", "Interaction"))) |>

make_cover_fig <- function(subset, fig_title, colour_palette, f_group) {

  p <- subset |>
    ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
    geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
    geom_point(size = 2) +
    labs(x = expression(log(Nitrogen~addition)~kg~ha^-1~y^-1)) +
    scale_colour_manual(name = "Effect", values = colour_palette) +
    scale_fill_manual(name = "Effect", values = colour_palette) +
    scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
    scale_shape_manual(name = "Warming", values = c(1, 16)) +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 16))

  if(f_group == "graminoid"){
    cover_figure <- p +
      labs(y = "Change in cover",
           title = fig_title) +
      facet_wrap( ~ grazing)

  } else if(f_group == "forb"){
    cover_figure <- p +
      labs(y = "Change in cover",
           title = fig_title) +
      theme(legend.position = "right")

  } else
    {

      cover_figure <- p +
        labs(y = "Change in richness",
             title = fig_title) +
        facet_wrap( ~ origSiteID) +
        theme(legend.position = "right",
              text = element_text(size = 20))

  }

  return(cover_figure)

}



# first panel for presentations
# panel1 <- cover_data %>%
#   filter(functional_group == "graminoid",
#          grazing =="Control",
#          origSiteID == "High alpine") %>%
#   ggplot(aes(x = Nitrogen_log, y = delta, colour = functional_group, linetype = warming, shape = warming)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
#        y = "Difference in cover (2021 - 2019)") +
#   scale_colour_manual(name = "", values = c("limegreen")) +
#   scale_linetype_manual(name = "warming", values = c("solid", "dashed")) +
#   scale_shape_manual(name = "warming", values = c(16, 1)) +
#   lims(y = c(-60, 80)) +
#   theme_minimal()
