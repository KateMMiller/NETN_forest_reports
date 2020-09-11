library(forestNETN)
library(tidyverse)
library(rgdal)
library(leaflet.minicharts)
library(leaflet)
library(htmltools)
#library(maptools)
importData()

#----------------------
#http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/
#----------------------


park_code = "MABI"
park_long_name = "Marsh-Billings-Rockefeller National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
                                         } else {"+init=epsg:26918"}


reg <- joinRegenData(park = park_code, speciesType = "native", canopyForm = 'all', 
                     from = 2016, to = 2019)

reg2 <- reg %>% group_by(Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord) %>% 
                summarise(sd15_30 = sum(seed15.30, na.rm = TRUE), 
                          sd30_100 = sum(seed30.100, na.rm = TRUE), 
                          sd100_150 = sum(seed100.150, na.rm = TRUE), 
                          sd150p = sum(seed150p, na.rm = TRUE),
                          sap = sum(sap.den, na.rm = TRUE),
                          totreg = sd15_30 + sd30_100 + sd100_150 + sd150p +sap,
                          .groups = 'keep') %>% ungroup()

coordinates(reg2) <- ~X_Coord + Y_Coord
proj4string(reg2) <- CRS(park_crs)

#reg3<-spTransform(reg3, CRS="+proj=longlat +datum=WGS84")
reg3<-spTransform(reg2, CRS="+init=epsg:4326")
mean_x = mean(reg3$X_Coord)
mean_y = mean(reg3$Y_Coord)

writeOGR(reg3, dsn = "./shapefiles", layer = paste0(park_code, "_regen_by_cycle_dd"), 
         driver = "ESRI Shapefile", overwrite_layer=T)
list.files("./shapefiles")

bound <- readOGR("./shapefiles/MABI_Boundary_2002.shp", 
                 layer = "MABI_Boundary_2002", GDAL1_integer64_policy = TRUE)

bound2 <- spTransform(bound, CRS="+init=epsg:4326")

park_cent <- rgeos::gCentroid(bound2)

writeOGR(bound2, dsn = './shapefiles', layer = "MABI_bound_dd", driver = "ESRI Shapefile")

npstile <- "//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q"

NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")


basemap <- leaflet(width = "100%", height = "800px") %>% 
  addTiles(urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
           attribution = NPSAttrib#, 
           #options = tileOptions(minZoom=12)
           ) %>% 
  setView(lng = park_cent$x, lat = park_cent$y, zoom = 15) #%>% 
  #setMaxBounds(lng1 = -77.203, lng2 = -77.286, lat1 = 39.859, lat2 = 39.771)
 # setMaxBounds(lng1 = -77.0, lng2 = -77.5, lat1 = 39.9, lat2 = 39.6)

  basemap

sd_colors = c("#D6D6FF", "#8F97E3", "#556CC9", "#244EAD", "#05e636")

reg3@data$label = as.numeric(substr(reg3@data$Plot_Name, 6, 8))
(reg3@coords[,1])

park_map <- basemap %>% 
  addMinicharts(
  reg3@coords[, 1], 
  reg3@coords[, 2],
  type = 'pie',
  chartdata = reg3@data[, c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap")],
  colorPalette = sd_colors,
  width = reg3@data$totreg*2,
  transitionTime = 0, 
  legend = FALSE) %>% 
  addScaleBar(position='bottomright') %>% 
  scaleBarOptions(maxWidth=10, metric=TRUE) #%>% 
park_map

