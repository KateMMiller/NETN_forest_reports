#-----------------------
# Source file to load all base layers, import data, and prepare for plotting
#-----------------------
library(tmap)
library(sf)
library(tidyverse)
library(forestNETN)
library(egg) # for set_panel_size; expose ggplot layout

options(scipen = 100, digits = 6)

source("./nudge_XY.R")

#importData()
#----- Next Steps -----
# 1. Set up markdown park templates, so MIMA and ROVA can be 2 layouts and ACAD can be 3
# 2. Add none symbol to pie_list for 0s
# 3. Figure out scale, so pies are scaled properly for small and big parks
# 4. Set up GIS layers for MIDN parks
# 5. Add std. var step in functions, rather than calculating outside

#----- Load spatial data -----
bounds <- st_read("./shapefiles/NETN_park_bounds_units_albers.shp")
plots <- st_read("./shapefiles/NETN_forest_plots_albers.shp")
#st_crs(plots) #5070
vegmap <- st_read("./shapefiles/NETN_vegmap_albers.shp")
veg_colors <- read.csv("./shapefiles/Vegmap_colors.csv")
units <- read.csv("./shapefiles/tbl_Alternative_Plot_Labels.csv")

#-----
# Columns that specify map controls
map_controls <- read.csv("./shapefiles/map_controls.csv")

map_controls$values <- factor(map_controls$values,
                              levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap", "nonereg",
                                         "cycle1", "cycle2", "cycle3", "cycle4", "noneregcyc", 
                                         "D1", "D2", "D3", "D4", "D5",
                                         "S1", "S2", "S3", "S4",
                                         "Cycle 1: 2006 – 2009", "Cycle 2: 2010 – 2013", "Cycle 3: 2014 – 2017", "Cycle 4: 2018 – 2020",
                                         "Conifer forest", "Conifer plantation", "Mixed forest", "Hardwood forest", "Mature hardwood forest",
                                         "Successional hardwood forest", "Spruce-fir forest", "Upland forest", "Exotic hardwood forest",
                                         "Forest gap", "Conifer woodland", "Mixed woodland", "Shrubland", "Forested wetland", 
                                         "Shrub wetland", "Open wetland", "Saltmarsh", "Headland", "Intertidal", "Beach", "Subalpine", 
                                         "Open field", "Open water", "Developed", "No data", park_code))


#----- Set expansion factors based on scale -----
park_bound <- bounds[bounds$Park == park_code, ]
park_veg <- vegmap[vegmap$Park == park_code, ]
park_plots <- plots[plots$Unit_Cd == park_code, ]

#----- Set up park controls -----
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}

park_layout <- ifelse(park_code %in% c("ACAD", "MABI", "MIMA", "ROVA"), "landscape", "portrait")

pie_expfac1 <- data.frame(park_code = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"),
                          pie_expfac = c(700, 50, 80, 80, 80, 25, 90, 22)) #~trial and error based on park's ideal map scale  

pie_expfac <- pie_expfac1$pie_expfac[pie_expfac1$park_code == park_code] 





