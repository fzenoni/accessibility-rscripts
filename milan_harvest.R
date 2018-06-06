library(cityr)
library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(tmap)
library(viridis)
library(tmaptools)

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

# Plot
tmap_mode('plot')

pal <- plasma(n = 100, direction=-1)

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
quantiles <- quantile(src$avg, 
                      probs = seq(0, 1, length.out = no_classes + 1))
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

src$avg_quantiles <- cut(src$avg, 
                         breaks = quantiles, 
                         labels = labels, 
                         include.lowest = T)


p <- ggplot() +
  geom_sf(data = st_sf(st_cast(st_union(city), "POLYGON")), colour = 'black') +
  geom_sf(data = src, aes(fill = avg_quantiles),
          col = 'white', lwd = 0.1
  ) +
  theme_map() +
  labs(x = NULL,
       y = NULL,
       title = "Accessibility to restaurants in Milan",
       subtitle = "Average time to go by foot to the 5 closest restaurants, 2018",
       caption = "Geometries: Copernicus Land Monitoring Service - Urban Atlas\nData: OpenStreetMap, OSRM") +
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


