### make vegetation figures


make_functional_group_biomass_figure <- function(biomass){

  biomass_fg_figure <- biomass |>
    filter(grazing != "Natural") |>
    mutate(warm_site = paste(origSiteID, warming, sep = " "),
           fun_group = factor(fun_group, levels = c("shrub", "graminoids", "cyperaceae", "forbs", "legumes", "bryophytes", "lichen", "litter"))) |>
    ggplot(aes(x = factor(Namount_kg_ha_y), y = productivity, fill = fun_group)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = c("darkgreen", "limegreen", "lawngreen", "plum4", "plum2", "orange", "red", "peru"), name = "") +
    labs(y = bquote(Productivity~(g~m^-2~y^-1)),
         x = bquote(Nitrogen~(kg~ha^-1~y^-1))) +
    facet_grid(warm_site ~ grazing) +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 12))

  return(biomass_fg_figure)


}

make_productivity_figure <- function(biomass){

  productivity_figure <- biomass %>%
    filter(!fun_group %in% c("litter")) %>%
    mutate(site_warm = paste0(origSiteID, warming)) %>%
    group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Namount_kg_ha_y, Nitrogen_log, grazing) %>%
    summarise(annual_productivity = sum(productivity)) %>%
    filter(annual_productivity < 700,
           warming == "Ambient",
           Namount_kg_ha_y == 0) |>
    ggplot(aes(y = annual_productivity, fill = grazing)) +
    geom_boxplot() +
    labs(y = bquote(Productivity~(g~m^-2~y^-1)),
         x = "") +
    scale_fill_viridis_d() +
    facet_grid(origSiteID ~ grazing, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")

  return(productivity_figure)

}

# 3 colour palette (grey, orange, pink)
#three_col_palette <- c("#999999", "#E69F00", "#CC79A7")

#        sign = if_else(is.na(sign),  "no effect", sign),
#        sign = recode(sign, W_sign = "Warming", NxW_sign = "Interaction"),
#        sign = factor(sign, levels = c("no effect", "Warming", "Interaction"))) |>

make_cover_fig <- function(subset, fig_title, colour_palette, facet_1) {

  p <- subset |>
    ggplot(aes(x = Nitrogen_log, y = delta, colour = sign, linetype = warming, shape = warming)) +
    geom_smooth(method = "lm", se = TRUE, aes(fill = sign)) +
    geom_point(size = 2) +
    labs(x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1))) +
    scale_colour_manual(name = "Effect", values = colour_palette) +
    scale_fill_manual(name = "Effect", values = colour_palette) +
    scale_linetype_manual(name = "Warming", values = c("dashed", "solid")) +
    scale_shape_manual(name = "Warming", values = c(1, 16)) +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 16))

  if(facet_1 == "functional_group"){
    cover_figure <- p +
      labs(y = "Change in cover",
           title = fig_title) +
      facet_grid(functional_group ~ grazing)

  } else
    {

      cover_figure <- p +
        labs(y = "Change in richness",
             title = fig_title) +
        facet_grid(origSiteID ~ grazing)

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
