library(sf)



map_df <- map_data("world", region = "Norway") %>%
  filter(is.na(subregion))

norway_map <- ggplot(map_df, aes(x = long, y = lat)) +
  geom_polygon(fill = "#8AAAA9", colour = "white") +
  geom_point(aes(x = 7.16990, y = 60.88019), colour = "#5B424B") +
  labs(x = NULL, y = NULL) +
  coord_map(projection = "conic", lat0 = 40) +
  theme_minimal()






#### Get Wolrdclim elevation data ####
library(raster)
elev <- getData('worldclim', var='alt', res=2.5)

#### WORLDMAP ####
elev.spdf <- as(elev, "SpatialPixelsDataFrame")
elev.df <- as.data.frame(elev.spdf)
# replace all values > 6000 with 6000 (everything above is not interesting)
elev.df$alt[elev.df$alt > 6000] <- 6000
range(elev.df)

# merge pop and site data set together
worldData <- rbind(new.studysite, data.frame(studysite = "Population", latitude_site = dat.pop$lat.pop, longitude_site = dat.pop$long.pop, studysite2 = "Population"))
worldData <- worldData[with(worldData, order(studysite2, decreasing = TRUE)), ]
worldData$studysite2 <- factor(worldData$studysite2)

# plot world map
w.map <- ggplot() +
  geom_raster(data = elev.df, aes(x=x, y=y, fill = alt)) +
  coord_equal() +
  labs(x="", y = "", fill = "Elevation\n", colour = "Study type", shape = "Study type", alpha = "Study type") +
  scale_y_continuous(limits = c(-60, 85)) +
  scale_color_manual(values = c(col.chamber, col.garden, col.molec, col.pop)) +
  scale_shape_manual(values = c(17, 17, 15, 16)) +
  scale_alpha_manual(values = c(0.9, 0.9, 0.9, 0.3)) +
  scale_fill_gradient(low = "grey0", high = "grey100", limits=c(-416,4000)) +
  geom_point(aes(x=longitude_site, y=latitude_site, colour  = studysite2, shape = studysite2, alpha = studysite2 ), data = worldData, size=2) +
  annotate("text", x = -190, y = 80, label = "A", size= 4) +
  theme(legend.position = "none")
w.map
