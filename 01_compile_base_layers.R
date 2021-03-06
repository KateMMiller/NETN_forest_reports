#-----------------------
# Source file to load all base layers, import data, and prepare for plotting
#-----------------------
library(sf)
library(tidyverse)
options(scipen = 100, digits = 6)
#-----------------------------
# Spatial data layers
#-----------------------------

#----- Load spatial data -----
#----- Set up park controls- set in 04_forest_summary_ROVA.R -----
# park_crs <- ifelse(park_code %in% c("ACAD", "MIMA"), "+init=epsg:26919", "+init=epsg:26918")
# zone <- ifelse(park_code %in% c("ACAD", "MIMA"), 19, 18)
# park_layout <- ifelse(park_code %in% c("ACAD", "MABI", "MIMA", "ROVA"), "landscape", "portrait")

bounds <- st_read(paste0("./shapefiles/NETN_park_bounds_", zone, ".shp"))

#plots <- st_read("./shapefiles/NETN_forest_plots_albers.shp")
vegmap <- st_read(paste0("./shapefiles/NETN_vegmap_", zone, ".shp"))

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
#park_plots <- plots[plots$Unit_Cd == park_code, ]

#--------------------
# Park and metric specific templates

#----- Create veg map legend for given park -----
#arrange by factor level set for map_controls
park_veg2 <- park_veg %>% arrange(veg_type) %>% st_drop_geometry(.) %>% 
                          rownames_to_column(., var = "x") %>% 
                          mutate(stroke = 1)

numcols <- ifelse(park_layout == "landscape", 2, 1) # Habitat type legend is 2 cols if landscape map

veg_leg  <- ggplot(data = park_veg2, 
                   aes(x = x, y = stroke, fill = veg_type))+ 
                       #stroke = stroke))+
  geom_point(alpha = 0.8, shape = 22, size = 5)+#7.5)+
  scale_fill_manual(values = park_veg2$fills,
                    name = "Habitat type", guide = "legend")+
  # scale_color_manual(values = c(rep("black", nrow(park_veg2)-1), "#7d7d7d"),
  #                    name = "Habitat type", guide = "legend")+
  #scale_size_identity(guide = "legend")+
  theme_void()+
  theme(legend.text = element_text(size = 10, margin = margin(r = 20)),
        legend.title = element_text(size = 11, family = "Arial", face = "bold"),
        legend.position = 'right',
        text = element_text(family = "Arial"),
        plot.background = element_blank(), panel.background = element_blank(),
        panel.border = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.grid = element_blank())+
  guides(fill = guide_legend(ncol = numcols))#,

fake_bound <- data.frame(x = 1, y = 1, group = "Park boundary")

bound_leg <- ggplot(data = fake_bound, aes(x, y, color = group))+
             geom_point(shape = 22, size = 5, #7.5, 
                        stroke = 1.5, fill = NA)+
             scale_color_manual(values = c("Park boundary" = "#6a6a6a"))+
             theme(legend.position = 'right', 
                   legend.title = element_blank(),
                   legend.text = element_text(size = 10), #margin = margin(r = 20)),
                   text = element_text(family = "Arial"),
                   plot.background = element_blank(), panel.background = element_blank(),
                   legend.background = element_blank(),
                   legend.key = element_blank())


#----- regsize pie legend -----
# fake dataset for leg
regsize_cols <- prep_sym_cols(df = map_controls, grp_var = "regsize")
regsize_symb <- prep_sym_shapes(df = map_controls, grp_var = "regsize") 

regsize_gen_df <- data.frame(totreg_std2 = c(rep(0.2, 5),0),
                             size_class = factor(c("sd15_30", "sd30_100", "sd100_150",
                                                   "sd150p", "sap", "nonereg")),
                             labs = c("Seedlings 15 \U2013 30cm", "Seedlings 30 \U2013 100cm",
                                      "Seedlings 100 \U2013 150cm", "Seedlings > 150cm",
                                      "Saplings (1 \U2013 9.9cm DBH)", "None present"))
# fake plot for leg
regsize_leg <- ggplot(regsize_gen_df,
                      aes(x = size_class, y = totreg_std2, 
                          fill = size_class, size = size_class,
                          shape = size_class))+
  geom_point()+
  scale_fill_manual(name = "Stem densities by size class",
                    values = regsize_cols, 
                    breaks = regsize_gen_df$size_class,
                    labels = regsize_gen_df$labs)+
  scale_shape_manual(name = "Stem densities by size class",
                     labels = regsize_gen_df$labs,
                     values = as.numeric(regsize_symb))+
  scale_size_manual(name = "Stem densities by size class",
                    labels = regsize_gen_df$labs,
                    #values = c(8, 8, 8, 8, 8, 3))+
                    values = c(5, 5, 5, 5, 5, 2))+
  theme_void()+
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"),
        text = element_text(family = "Arial"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank())

regsize_leg




