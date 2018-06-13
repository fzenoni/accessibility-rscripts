# BEFORE EXECUTING THIS SCRIPT 
# YOU HAVE TO TURN ON THE HTTP API
# osrm-routed map.osrm

library(curl)
library(jsonlite)
library(googlePolylines)

source <- src_subset %>% filter(id == 1877) %>%
  st_transform(crs = 3857) %>% st_centroid() %>%
  st_transform(crs = 4326) %>% st_coordinates()
destination <- poi[c(1282,2124,110,1261,646),] %>% st_coordinates()

GPStoRoute <- function (osrm_server, service, origin_lon, origin_lat,
                        destination_lon, destination_lat) {
  
  url <- paste0(osrm_server, '/route/v1/', service, '/', origin_lon, ",", origin_lat,
                ";", destination_lon, ",", destination_lat, "?overview=full")
  
  req <- curl_fetch_memory(url)
  content <- rawToChar(req$content) %>% fromJSON
  route <- decode(content$routes$geometry)[[1]] %>%
    st_as_sf(coords = c('lon', 'lat'))
  
  route <- do.call(c, st_geometry(route)) %>% st_cast('LINESTRING')
  st_sfc(route, crs = 4326)
}

GPStoDuration <- function (osrm_server, service, origin_lon, origin_lat,
                           destination_lon, destination_lat) {
  
  url <- paste0(osrm_server, '/route/v1/', service, '/', origin_lon, ",", origin_lat,
                ";", destination_lon, ",", destination_lat, "?overview=full")
  
  req <- curl_fetch_memory(url)
  content <- rawToChar(req$content) %>% fromJSON
  duration <- content$routes$duration
  duration
}

# Examples
GPStoRoute('http://0.0.0.0:5000', 'walking', source[1], source[2],
           destination[4,1], destination[4,2])

GPStoDuration('http://0.0.0.0:5000', 'walking', source[1], source[2],
              destination[4,1], destination[4,2])

bbox_milan_detail <- matrix(c(9.223, 45.4711, 9.23, 45.4747), nrow = 2)
milan_map_detail <- ggmap::get_map(location = bbox_milan_detail, source = 'stamen', maptype = 'watercolor', zoom = 17)

# Draw small pictures
drawRoute <- function(i, color = viridis(6)[1]) {
  route <- GPStoRoute('http://0.0.0.0:5000', 'walking', source[1], source[2],
                      destination[i,1], destination[i,2])
  duration <- GPStoDuration('http://0.0.0.0:5000', 'walking', source[1], source[2],
                            destination[i,1], destination[i,2])
  
  label <- data.frame(text = duration, x = 9.225, y = 45.472)
  
  s <- ggmap::ggmap(milan_map_detail) +
    # geom_sf(data = filter(src_subset, id==1877), fill = color,
    #         alpha = 0.7, lwd = 0, inherit.aes = FALSE) +
    # geom_point(data = data.frame(source), aes(X,Y), shape = 4, size = 8, color = 'darkred') +
    geom_point(data = data.frame(destination)[i,], aes(X,Y), shape = 15, size = 8, color = 'darkred') +
    geom_path(data = data.frame(st_coordinates(route)), aes(X, Y),
              inherit.aes = FALSE, colour = color, size = 2,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    geom_label(data = label,
               aes(x = 9.225, y = 45.472, label = paste(text, 's')),
               family = 'Ubuntu Regular', label.size = 1,
               size = 9, nudge_x = -0.001, nudge_y = +0.0022) +
    # coord_sf(xlim = c(9.223, 9.23),
    #          ylim = c(45.4711, 45.4747), default = TRUE) +
    scalebar(data=filter(src_subset, id==1877), dist = 100, dist_unit = 'm',
             dd2km = TRUE, model = 'WGS84', location = 'bottomright',
             st.size = 4, height = 0.1, st.dist = 0.07,
             st.bottom = FALSE,
             st.color = color, box.color = color,
             box.fill = c(color, 'white'),
             anchor = c(x = source[1]+0.0025, y = source[2]-0.0026)) +
    # north(data = filter(src_subset, id==1877), scale = 1, symbol = 3,
    #       anchor =  c(x = source[1]+0.0025, y = source[2]+0.0005)) +
    theme_map()
  # ggsn::scalebar(data=filter(src_subset, id == 1877), dist = 0.05, dist_unit = 'm',
  #          dd2km = TRUE, model = 'WGS84', location = 'bottomright',
  #          st.size = 1, height = 0.01, st.dist = 0.01,
  #          st.bottom = FALSE, anchor =
  #            c(x = xmax - 0.0015, y = ymin + 0.001))
  
  s
}
drawRoute(1)

xmin_detail <- bbox_milan_detail[1,1]
xmax_detail <- bbox_milan_detail[1,2]
ymin_detail <- bbox_milan_detail[2,1]
ymax_detail <- bbox_milan_detail[2,2]

milan_detail_pol = st_sf(st_sfc(st_polygon(list(cbind(c(xmin_detail, xmax_detail, xmax_detail, xmin_detail, xmin_detail),
                                                      c(ymin_detail, ymin_detail, ymax_detail, ymax_detail, ymin_detail))))),
                         crs = 4326)
