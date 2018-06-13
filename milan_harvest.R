library(cityr)
library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
# library(tmap)
library(viridis)
library(tmaptools)
library(ggmap)
library(ggsn)

# Set custom overpass URL
osmdata::set_overpass_url("http://40.68.166.16/api/interpreter")

# Load/Extract Milan boundaries
poly_name <- 'Milan8_poly.rds'
if(!file.exists(file.path('data', poly_name))) {
  poly_path <- file.path('data', poly_name)
  city <- get_city_boundary(name = 'Milan',
                            adm_level = '8')
  saveRDS(city, poly_path)
} else {
  city <- readRDS(file.path('data', poly_name))
}

# Load boundaries
# bound <- read_sf('data/Milan_Urban_Atlas/Shapefiles/Boundary2012_IT002L2_MILANO.shp')
# st_crs(bound) <- 3035

# milan <- city %>% filter(name == 'Milano')

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

urban_indexes <- grep('urban fabric', unique(ua_city_all$ITEM2012))
urban_fabric <- unique(ua_city_all$ITEM2012)[urban_indexes]
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
# Run OSRM C++ script and get output
####

###
# Run analysis.R
data <- fread('../osrm-application/output.csv')

# Compute average duration for 5 closer destinations
avg <- data[, .(avg = mean(head(sort(V2), 5))), by = V1]
# avg <- avg5[, .(avg = mean(closer5)), by = V1]
setnames(avg, 'V1', 'id')
avg[, avg := avg/60]
###



# src <- output[type == 'S']

ua_city <- ua_city %>% mutate(id := seq.int(nrow(ua_city)) - 1)
src <- merge(ua_city, avg, by = 'id')
src <- src %>% st_transform(crs = 4326)

xmin <- 9.1792
ymin <- 45.4511
xmax <- 9.2377
ymax <- 45.5030

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
      panel.border = element_rect(colour = 'black', fill = NA, size = 1),
      ...
    )
}


# Pretty breaks
pretty_breaks <- c(0.3, 2, 3, 5, 10)
# find the extremes
minVal <- min(src_subset$avg, na.rm = T)
maxVal <- max(src_subset$avg, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels, round(brks[idx+1], 2))
}
labels <- labels[1:length(labels)-1]

# Breaks
src_subset$brks <- cut(src_subset$avg, 
                       breaks = brks, 
                       include.lowest = TRUE, 
                       labels = labels)

brks_scale <- levels(src_subset$brks)
labels_scale <- rev(brks_scale)

# Quantiles
# no_classes <- 6
# quantiles <- quantile(src_subset$avg, 
#                       probs = seq(0, 1, length.out = no_classes + 1))
# labels <- c()
# for(idx in 1:length(quantiles)){
#   labels <- c(labels, paste0(round(quantiles[idx], 2), 
#                              " – ", 
#                              round(quantiles[idx + 1], 2)))
# }
# labels <- labels[1:length(labels)-1]
# 
# src_subset$avg_quantiles <- cut(src_subset$avg, 
#                          breaks = quantiles, 
#                          labels = labels, 
#                          include.lowest = T)

# From now on: mind that maps must be projected in 3857,
# NOT kept in lat-lon 4326!!!!!

milan_small <- c(xmin, ymin, xmax, ymax)
bbox_milan_small <- matrix(milan_small, ncol = 2)
if(!file.exists('data/milan_map.Rds')) {
  milan_map <- ggmap::get_map(location = bbox_milan_small, source = 'stamen', maptype = 'toner-hybrid', zoom = 16)
  saveRDS(milan_map, 'data/milan_map.Rds')
} else {
  milan_map <- readRDS('data/milan_map.Rds')
}

poi_df <- data.frame(st_coordinates(poi))

# Draw
# SPECIAL INSTRUCTIONS:
# The row that includes milan_detail_pol boundary will not be available unless
# the routing.R script is executed
p <- ggmap::ggmap(milan_map) +
  geom_sf(data = src_subset, aes(fill = brks),
          alpha = 0.7, col = 'white', lwd = 0.1, inherit.aes = FALSE) +
  # coord_sf(xlim = c(bbox_milan_small[1,1], bbox_milan_small[1,2]),
  #          ylim = c(bbox_milan_small[2,1], bbox_milan_small[2,2]), default = TRUE) +
  geom_point(data = poi_df, aes(X,Y), inherit.aes = FALSE, size = 0.5, shape = 15, colour = 'red4') +
  geom_sf(data=st_boundary(milan_detail_pol), inherit.aes = FALSE, colour = 'red', size = 1, linetype = 1) +
  scalebar(src_subset, dist = 0.5, dist_unit = 'km',
           dd2km = TRUE, model = 'WGS84', location = 'bottomright',
           st.size = 4, height = 0.01, st.dist = 0.02,
           st.bottom = FALSE, anchor =
             c(x = xmax - 0.0032, y = ymin + 0.002)) +
  north(src_subset, symbol = 12) +
  theme_map() +
  theme(legend.position = 'bottom', legend.box = 'vertical')
  # labs(x = NULL,
  #      y = NULL,
  #      title = "Accessibility to restaurants in Milan",
  #      subtitle = "Average time to go by foot to the 5 closest restaurants",
  #      caption = "Geometries: Copernicus Land Monitoring Service - Urban Atlas\nData: OpenStreetMap, OSRM")

q <- p +
  # scale_color_manual(
  #   name = element_blank(),
  #   na.translate = FALSE,
  #   labels = 'Restaurant',
  #   values = 'darkgreen') +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(viridis(8)[2:7]),
    breaks = rev(brks_scale),
    name = "Average time (min)",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )
q


