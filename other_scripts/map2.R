library(rnaturalearth)
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")

norway <- ne_countries(scale = 10, returnclass = "sf", country = "Norway")

norway_fig <- ggplot() +
  geom_sf(data = norway, fill = "lavenderblush3", colour = "cornsilk4") +
  coord_sf(xlim = c(5, 30), ylim = c(57, 71), ndiscr = 0) +
  geom_point(aes(x = 7.16990, y = 60.88019), colour = "plum4", size = 3) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# read in data
norway_dem <- rast("data/wc2.1_30s_elev.tif")
# crop to vestland and convert to sf object
vestland_extent <- ext(5, 8, 60, 61.5)
vestland_dem <- crop(norway_dem, vestland_extent) |>
  as.points() |>
  st_as_sf()

#rename the data layer
names(vestland_dem)[1] <- "Elevation"
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
# plot
ggplot(vestland_dem) +
  geom_sf(aes(colour = Elevation)) +
  geom_point(aes(x = 7.16990, y = 60.88019), colour = "plum4") +
  geom_point(aes(x = 7.16800, y = 60.86183), colour = "plum4") +
  geom_point(aes(x = 7.19504, y = 60.85994), colour = "plum4") +
  labs(x = "Longitude", y = "Latidude") +
  scale_colour_gradientn(colours = pal) +
  theme_minimal()




