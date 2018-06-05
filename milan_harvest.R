library(cityr)
library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(tmap)
library(viridis)

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

milan <- city %>% filter(name == 'Milano')

bbox <- st_bbox(city)
bbox <- matrix(bbox, nrow=2)

query <- bbox %>% opq() %>% add_osm_feature("amenity", "restaurant") %>% 
  osmdata_sf()

poi <- cityr::aggregate_points(query)

plot(st_as_sfc(st_bbox(city)))
plot(st_union(city), add = T)
plot(st_transform(poi, 3035), col = 'red', add = T)

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

# Run OSRM and get output
src <- output[type == 'S']

ua_city <- ua_city %>% mutate(id := seq.int(nrow(ua_city)))
src <- merge(ua_city, avg, by.x = 'id', by.y = 'V1')

# plot(st_union(city))
# plot(src['avg'], border = 0, add = T)

pal <- plasma(n = 100, direction=-1)

tm_shape(city) + tm_fill() +
  # tm_shape(ua_city_all) + tm_fill() +
  tm_shape(src) + tm_fill(col = 'avg', palette=pal, style = 'quantile') +
  # tm_grid(n.x = 5, n.y = 5, projection = 'longlat') +
  tm_scale_bar(position = c("right", "bottom"))
