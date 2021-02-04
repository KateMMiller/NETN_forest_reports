#-----------------------
# Source file to load all base layers, import data, and prepare for plotting
#-----------------------
library(sf)

options(scipen = 100, digits = 6)

#----- Load spatial data -----
bounds <- st_read("./shapefiles/NETN_park_bounds_units_albers.shp")
plots <- st_read("./shapefiles/NETN_forest_plots_albers.shp")
#st_crs(plots) #5070
vegmap <- st_read("./shapefiles/NETN_vegmap_albers.shp")
sort(unique(vegmap$veg_type ))
vegmap$veg_type <- factor(vegmap$veg_type,
                          levels = c("Conifer forest", "Conifer plantation", "Mixed forest", "Hardwood forest", "Mature hardwood forest",
                                     "Successional hardwood forest", "Spruce-fir forest", "Upland forest", "Exotic hardwood forest",
                                     "Forest gap", "Conifer woodland", "Mixed woodland", "Shrubland", "Forested wetland", 
                                     "Shrub wetland", "Open wetland", "Saltmarsh", "Headland", "Intertidal", "Beach", "Subalpine", 
                                     "Open field", "Open water", "Developed", "No data"))

veg_colors <- read.csv("./shapefiles/Vegmap_colors.csv")
units <- read.csv("tbl_Alternative_Plot_Labels.csv")

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


#----- Create veg map legend for given park -----
#arrange by factor level set for map_controls
park_veg2 <- park_veg %>% arrange(veg_type) %>% st_drop_geometry(.) %>% 
                          rownames_to_column(., var = "x")
 
head(park_veg2)

numcols <- ifelse(park_layout == "landscape", 2, 1) # Habitat type legend is 2 cols if landscape map

veg_leg  <- ggplot(data = park_veg2, 
                   aes(x = x, y = x, fill = veg_type))+
  geom_point(alpha = 0.8, shape = 22, size = 7.5)+
  scale_fill_manual(values = park_veg2$fills,
                    name = "Habitat type")+
  theme_void()+
  theme(legend.text = element_text(size = 11, margin=margin(r = 20)),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = 'right',
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank())+
  guides(fill = guide_legend(ncol = numcols)) #make legend 2 columns

veg_leg

