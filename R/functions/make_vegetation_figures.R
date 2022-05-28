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

