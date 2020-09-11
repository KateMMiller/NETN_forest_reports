library(rgdal)


park_code = "MABI"
park_long_name = "Marsh-Billings-Rockefeller National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}

coordinates(df) <- ~X_Coord + Y_Coord
proj4string(df) <- CRS(park_crs)

reg3 <- spTransform(reg2, CRS="+init=epsg:4326")
mean_x = mean(df$X_Coord)
mean_y = mean(df$Y_Coord)

writeOGR(df, dsn = "./shapefiles", layer = paste0(park_code, "_regen_by_cycle_dd"), 
         driver = "ESRI Shapefile", overwrite_layer=T)

bound <- readOGR("./shapefiles/MABI_Boundary_2002.shp", 
                 layer = "MABI_Boundary_2002", GDAL1_integer64_policy = TRUE)

bound2 <- spTransform(bound, CRS="+init=epsg:4326")

park_cent <- rgeos::gCentroid(bound2)

writeOGR(bound2, dsn = './shapefiles', layer = "MABI_Boundary_2002_dd", driver = "ESRI Shapefile")

