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
milan_map_detail <- ggmap::get_map(location = bbox_milan_detail, source = 'stamen', maptype = 'toner', zoom = 17)

# Draw small pictures
drawRoute <- function(i, color = viridis(6)[4]) {
  route <- GPStoRoute('http://0.0.0.0:5000', 'walking', source[1], source[2],
                      destination[i,1], destination[i,2])
  duration <- GPStoDuration('http://0.0.0.0:5000', 'walking', source[1], source[2],
                            destination[i,1], destination[i,2])
  
  s <- ggmap::ggmap(milan_map_detail) +
    geom_sf(data = filter(src_subset, id==1877), fill = color,
            alpha = 0.7, col = 'white', lwd = 0.1, inherit.aes = FALSE) +
    geom_sf(data=st_sfc(route), inherit.aes = FALSE, colour = 'darkred', size = 4) +
    # coord_sf(xlim = c(9.223, 9.23),
    #          ylim = c(45.4711, 45.4747), default = TRUE) +
    scalebar(data=filter(src_subset, id==1877), dist = 100, dist_unit = 'm',
             dd2km = TRUE, model = 'WGS84', location = 'bottomright',
             st.size = 4, height = 0.05, st.dist = 0.07,
             st.bottom = FALSE,
             st.color = 'darkred',
             box.fill = c('darkred', 'white'),
             anchor = c(x = source[1]+0.0025, y = source[2]-0.0026)) +
    # north(data = filter(src_subset, id==1877), symbol = 12) +
    theme_map()
    # ggsn::scalebar(data=filter(src_subset, id == 1877), dist = 0.05, dist_unit = 'm',
    #          dd2km = TRUE, model = 'WGS84', location = 'bottomright',
    #          st.size = 1, height = 0.01, st.dist = 0.01,
    #          st.bottom = FALSE, anchor =
    #            c(x = xmax - 0.0015, y = ymin + 0.001))
    
  s
}
drawRoute(1)
