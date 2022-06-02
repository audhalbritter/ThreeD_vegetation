### make outputs

# Climate
tar_load(annual_climate_figure)
ggsave(filename = "output/annual_climate.png", annual_climate_figure, dpi = 300, width = 8, height = 7)

tar_load(daily_climate_figure)
ggsave(filename = "output/climate.png", daily_climate_figure, dpi = 300, width = 8, height = 7)


# Vegetation
tar_load(biomass_fun_group)
ggsave(filename = "output/biomass_fun_group.png", biomass_fun_group, dpi = 300, width = 10, height = 8)

tar_load(productivity_figure)
ggsave(filename = "output/productivity.png", productivity_figure, dpi = 300, width = 6, height = 4)

# Cover
tar_load(cover_subalpine_figure)
ggsave(cover_subalpine_figure, filename = "output/cover_subalpine.png", width = 8, height = 6)

tar_load(cover_alpine_figure)
ggsave(cover_alpine_figure, filename = "output/cover_alpine.png", width = 8, height = 6)

tar_load(cover_cn_alpine_figure)
ggsave(cover_cn_alpine_figure, filename = "output/cover_cn_alpine.png", width = 8, height = 6)

tar_load(cover_cn_subalpine_figure)
ggsave(cover_cn_subalpine_figure, filename = "output/cover_cn_subalpine.png", width = 8, height = 6)


library(tidyverse)
library(targets)
biomass %>%
  filter(!fun_group %in% c("litter")) %>%
  # summarise all functional groups
  group_by(origSiteID, origBlockID, origPlotID, turfID, destSiteID, destBlockID, destPlotID, warming, Namount_kg_ha_y, grazing) %>%
  summarise(annual_productivity = sum(productivity)) %>%
  filter(grazing != "Natural") |>
  ggplot(aes(x = log(Namount_kg_ha_y), y = annual_productivity, colour = warming, linetype = warming, shape = warming)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = bquote(Nitrogen~(kg~ha^-1~y^-1)),
           y = "Productivity in g per year") +
      scale_colour_manual(values = c("grey", "red")) +
      scale_linetype_manual(name = "warming", values = c("dashed", "solid")) +
      scale_shape_manual(name = "warming", values = c(16, 1)) +
      facet_grid(origSiteID ~ grazing, scales = "free_y") +
      theme_minimal()






biomass_raw |>
  filter(grazing == "I",
         #Namount_kg_ha_y %in% c(100),
         !is.na(year),
         fun_group != "litter") |>
  group_by(cut, warming, Namount_kg_ha_y, origSiteID, turfID, year) |>
  summarise(biomass = sum(biomass)) |>
  ggplot(aes(x = as.factor(cut), y = biomass, shape = warming, colour = Namount_kg_ha_y)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.75) +
  scale_colour_viridis_c(end = 0.9, option = "inferno") +
  scale_shape_manual(values = c(1, 17)) +
  facet_grid(origSiteID ~ year)


cover |> distinct(species)
  group_by(turfID, origSiteID, warming, Namount_kg_ha_y, grazing) |>
  mutate(n = n()) |>
  filter(Namount_kg_ha_y == 0,
         grazing == "Control") |>
  ungroup() |>
  group_by(origSiteID, warming) |>
  summarize(richness = mean(n))



  threshold <-  80

  cover |>
    select(turfID, species, cover) |>
    group_by(turfID) |>
    arrange(turfID, -cover) |>
    mutate(cumsum = cumsum(cover)) |>
    filter(cumsum <= threshold)

