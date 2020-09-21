library(forestNETN)
library(tidyverse)
library(rgdal)
library(sf)

importData()

NETN_plot_data <- joinLocEvent(park = "all", from = 2016, to = 2019)
NETN_plot_19 <- NETN_plot_data %>% filter(Unit_Code %in% c("ACAD", "MIMA"))
NETN_plot_18 <- NETN_plot_data %>% filter(!Unit_Code %in% c("ACAD", "MIMA"))

coordinates(NETN_plot_19) <- ~X_Coord + Y_Coord
proj4string(NETN_plot_19) <- CRS("+init=epsg:26919")

coordinates(NETN_plot_18) <- ~X_Coord + Y_Coord
proj4string(NETN_plot_18) <- CRS("+init=epsg:26918")

NETN_plot_19dd <- spTransform(NETN_plot_19, CRS="+init=epsg:4326")
NETN_plot_18dd <- spTransform(NETN_plot_18, CRS="+init=epsg:4326")

NETN_plots_dd <- rbind(NETN_plot_19dd, NETN_plot_18dd)

writeOGR(NETN_plots_dd, dsn = "./shapefiles", layer = "NETN_forest_plots_dd", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)



bound <- readOGR("./shapefiles/MABI_Boundary_2002_dd.shp", 
                 layer = "MABI_Boundary_2002_dd", GDAL1_integer64_policy = TRUE)

regen_df <- regen_df %>% mutate(lat_nudge = case_when(Plot_Number == "017" ~ lat + 0.0001,
                                                      Plot_Number == "011" ~ lat - 0.0001,
                                                      TRUE ~ lat), 
                                long_nudge = case_when(Plot_Number == "017" ~ long + 0.0001,
                                                       Plot_Number == "011" ~ long - 0.0001,
                                                       TRUE ~ long),
                                totreg_std2 = ifelse(totreg_std <0.1, 0.1, totreg_std),
)

table(veg_shp@data$Map_Group)

regen_colors = c("sd15_30" = "#D6D6FF",
                 "sd30_100" = "#8F97E3",
                 "sd100_150" = "#556CC9",
                 "sd150p" = "#244EAD",
                 "sap" = "#05e636")

veg_colors = c("Conifer forest" = "#a0725e",
               "Conifer plantation" = "#b39267",
               "Developed" = "#c2c2c2",
               "Forested wetland" = "#9577a6",
               "Hardwood forest" = "#7ea46b",
               "Mixed forest" = "#55785e",
               "Open field" = "#f5f0b0",
               "Open water" = "#9bd2ef",
               "MABI" = NA)

none_colors = c("MABI" = "#ff7f00")

map_colors = c(regen_colors, veg_colors, none_colors)


veg_shp2 <- fortify(veg_shp, region = "Map_Group")
bound2 <- fortify(bound, region = "Unit_ID")