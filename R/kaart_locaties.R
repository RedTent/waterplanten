library(sf)
library(leaflet)
library(dplyr)

con <- connect_database()

DBI::dbListTables(con)

locs <- dplyr::tbl(con, "Locations")

locs %>% 
  dplyr::collect() %>% 
  dplyr::filter(!is.na(X_RD)) %>% 
  sf::st_as_sf(coords = c("X_RD", "Y_RD"), crs = 28992, remove = FALSE) %>% 
  sf::st_transform(crs = 4326) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(label = ~paste(Location_Name, Landscape_NL), stroke = FALSE, fillOpacity = 0.9)

odbc::dbDisconnect(con)
