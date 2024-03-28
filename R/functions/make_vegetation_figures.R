### make vegetation figures

make_vegetation_figure <- function(dat1,
                                   x_axis,
                                   yaxislabel,
                                   colourpalette, linetypepalette, shapepalette,
                                   facet_2,
                                   # predictions
                                   dat2){

dat2 <- dat2 |>
    rename(.x_axis = {{x_axis}})

plot <- dat1 |>
    rename(.x_axis = {{x_axis}}) |>
    ggplot(aes(x = .x_axis,
               y = .response,
               color = warming,
               linetype = grazing,
               shape = grazing)) +
    # zero line
    geom_hline(yintercept = 0, colour = "lightgrey") +
    # CI from prediction
    geom_ribbon(data = dat2, aes(y = prediction, ymin = lwr,
                    ymax = upr,
                    fill = warming),
                alpha = 0.1,
                linetype = 0) +
    geom_point(size = 2) +
    # prediction line
    geom_line(data = dat2, aes(y = prediction), linewidth = 0.5) +
    labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
         y = yaxislabel) +
    # scales
    scale_colour_manual(name = "Warming", values = colourpalette) +
    scale_fill_manual(name = "Warming", values = colourpalette) +
    scale_linetype_manual(name = "Grazing", values = linetypepalette) +
    scale_shape_manual(name = "Grazing", values = shapepalette) +
    # change labels to real values
    scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
    # facet
    facet_grid(origSiteID ~ .data[[facet_2]], scales = "free") +
    theme_bw() +
    theme(legend.position = "top",
          legend.box ="vertical",
          text = element_text(size = 12))

  if(is.na(facet_2)){
    plot + facet_grid(origSiteID ~ "", scales = "free")
  } else {
    plot
  }

}




### biomass and productivity

make_functional_group_biomass_figure <- function(biomass){

  biomass |>
    # filter only 2022. Filter for only one cut at peak growing season.
    filter(year == 2022,
           grazing == "Control" & cut == 3|
             grazing %in% c("Medium", "Intensive") & cut == 4) |>
    mutate(warm_site = paste(origSiteID, warming, sep = " "),
           fun_group = factor(fun_group, levels = c("shrub", "graminoids", "cyperaceae", "forbs", "legumes", "bryophytes", "litter"))) |>
    ggplot(aes(x = factor(Namount_kg_ha_y), y = biomass_scaled, fill = fun_group)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = c("darkgreen", "limegreen", "lawngreen", "plum4", "plum2", "orange", "peru"), name = "") +
    labs(y = "Proportional functional group composition",
         x = bquote(Nitrogen~(kg~ha^-1~y^-1))) +
    facet_grid(origSiteID * warming ~ grazing) +
    theme_bw() +
    theme(legend.position = "top",
          text = element_text(size = 12))

}

make_productivity_figure <- function(biomass){

  productivity_figure <- biomass |>
    filter(#grazing != "Natural",
           !fun_group %in% c("litter")) %>%
    # summarise the cuts to get annual productivity
    group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year) %>%
    summarise(annual_productivity = sum(biomass_scaled)) %>%
    filter(annual_productivity < 700,
           warming == "Ambient",
           Namount_kg_ha_y == 0) |>
    ggplot(aes(x = factor(year), y = annual_productivity, fill = grazing)) +
    geom_boxplot() +
    labs(y = bquote(Productivity~(g~m^-2~y^-1)),
         x = "") +
    scale_fill_viridis_d() +
    facet_grid(origSiteID ~ grazing, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")

  return(productivity_figure)

}
