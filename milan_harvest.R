library(cityr)
library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
# library(tmap)
library(viridis)
# library(tmaptools)
library(ggmap)

# Set custom overpass URL
osmdata::set_overpass_url("http://40.68.166.16/api/interpreter")

# Load/Extract Milan boundaries
poly_name <- 'Milan10_poly.rds'
if(!file.exists(file.path('data', poly_name))) {
  poly_path <- file.path('data', poly_name)
  city <- get_city_boundary(name = 'Milan',
                            adm_level = '10')
  saveRDS(city, poly_path)
} else {
  city <- readRDS(file.path('data', poly_name))
}

# Load boundaries
# bound <- read_sf('data/Milan_Urban_Atlas/Shapefiles/Boundary2012_IT002L2_MILANO.shp')
# st_crs(bound) <- 3035

milan <- city %>% filter(name == 'Milano')

bbox <- st_bbox(city)
# create some kind of buffer to mitigate border effects
bbox['xmin'] <- bbox['xmin'] - 0.1
bbox['ymin'] <- bbox['ymin'] - 0.1
bbox['xmax'] <- bbox['xmax'] + 0.1
bbox['ymax'] <- bbox['ymax'] + 0.1
bbox <- matrix(bbox, nrow=2) 

query <- bbox %>% opq() %>% add_osm_feature("amenity", "restaurant") %>% 
  osmdata_sf() %>% unique_osmdata()

poi <- cityr::aggregate_points(query)

plot(st_as_sfc(st_bbox(city)))
plot(st_union(city), add = T)
plot(poi['osm_id'], col = 'red', add = T)

# Load Urban Atlas
ua <- read_sf('data/Milan_Urban_Atlas/Shapefiles/IT002L2_MILANO_UA2012.shp')
st_crs(ua) <- 3035

# Subset urban atlas to "city"
city <- st_transform(city, crs = 3035)
ua_city_all <- ua[city,]

urban_fabric <- unique(ua_city_all$ITEM2012)[1:5]
ua_city <- ua_city_all %>% filter(ITEM2012 %in% urban_fabric)

# Building the output
sources <- st_transform(st_centroid(ua_city), crs = 4326)
destinations <- poi

sources <- st_coordinates(sources) %>% as.data.frame
sources <- sources %>% mutate(type = 'S')

destinations <- st_coordinates(destinations) %>% as.data.frame
destinations <- destinations %>% mutate(type = 'D')

output <- rbindlist(list(sources, destinations))
output[, id := seq.int(nrow(output)) - 1]
setcolorder(output, c('id', 'type', 'X', 'Y'))

write.table(output, 'input.txt', sep = ' ', row.names = FALSE,
            col.names = FALSE, quote = FALSE)

####
# Run OSRM and get output
####

###
# Run analysis.R
###

src <- output[type == 'S']

ua_city <- ua_city %>% mutate(id := seq.int(nrow(ua_city)))
src <- merge(ua_city, avg, by = 'id')
src <- src %>% st_transform(crs = 4326)

xmin <- 9.1462
ymin <- 45.4386
xmax <- 9.2047
ymax <- 45.4911

milan_pol = st_sf(st_sfc(st_polygon(list(cbind(c(xmin, xmax, xmax, xmin, xmin),
                                   c(ymin, ymin, ymax, ymax, ymin))))), crs = 4326)
src_subset <- st_intersection(src, milan_pol)
plot(src_subset['avg'])

# pal <- plasma(n = 100, direction=-1)

# tiles <- read_osm(bbox_milan, zoom = 15, type = 'osm-public-transport')

# tm_shape(tiles) + tm_raster() +
# tm_shape(ua_city_all) + tm_fill() +
# tm_shape(src) + tm_fill(col = 'avg', palette = pal, style = 'quantile') +
# tm_shape(poi) + tm_dots() +
# # tm_grid(n.x = 5, n.y = 5, projection = 'longlat') +
# tm_scale_bar(position = c("right", "bottom"))

# Try with ggplot2
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}




# Quantiles
no_classes <- 6
quantiles <- quantile(src_subset$avg, 
                      probs = seq(0, 1, length.out = no_classes + 1))
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

src_subset$avg_quantiles <- cut(src_subset$avg, 
                         breaks = quantiles, 
                         labels = labels, 
                         include.lowest = T)


milan_small <- c(xmin, ymin, xmax, ymax)
bbox_milan_small <- matrix(milan_small, ncol = 2)
milan_map <- ggmap::get_map(location = bbox_milan_small, source = 'stamen', maptype = 'toner-hybrid', zoom = 15)
# milan_map <- ggmap::get_map(location = milan_small, source = 'osm', zoom = 12)
# milan_map <- read_osm(bbox_milan_small, zoom = 16, type = 'osm')

p <- ggmap::ggmap(milan_map) +
  # geom_sf(data = st_sf(st_cast(st_union(city), "POLYGON")), colour = 'black', inherit.aes = FALSE) +
  geom_sf(data = st_transform(src_subset, crs = 4326), aes(fill = avg_quantiles),
          alpha = 0.7, col = 'white', lwd = 0.1, inherit.aes = FALSE) +
  coord_sf(xlim = c(bbox_milan_small[1,1], bbox_milan_small[1,2]),
           ylim = c(bbox_milan_small[2,1], bbox_milan_small[2,2]), default = TRUE) +
  # geom_sf(data = poi, inherit.aes = FALSE) +
  theme_map() +
  labs(x = NULL,
       y = NULL,
       title = "Accessibility to restaurants in Milan",
       subtitle = "Average time to go by foot to the 5 closest restaurants, 2018",
       caption = "Geometries: Copernicus Land Monitoring Service - Urban Atlas\nData: OpenStreetMap, OSRM") +
  # scale_alpha_continuous(guide='none') +
  scale_fill_viridis(
    option = "plasma",
    name = "Average time (min)",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = F
    ))

p


