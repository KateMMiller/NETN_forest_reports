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

#----- Set up pie charts -----
# Function to create formatted color list
prep_sym_cols <- function(df, grp_var){
  map_colors_df <- df[df$group == grp_var, c('values','fills')]
  map_colors1 <- t(map_colors_df)
  colnames(map_colors1) <- map_colors1[1, ]
  map_colors <- map_colors1[-1, ]
  return(map_colors)
}

regsize_cols <- prep_sym_cols(map_controls, "regsize")

# Function to create ggplot pie charts
pie_fun <- function(df, plotname, y_var, grp_var, std_var){
  grp_var <- enquo(grp_var)
  y_var <- enquo(y_var)

  df2 <- df[df$Plot_Name == plotname,] %>% ungroup()
  pie_exp1 <- df2 %>% select(!!std_var) %>% unique() %>% as.numeric()
  pie_exp <- pie_min + (pie_max - pie_min)*pie_exp1 # expansion factors for pies range(0.7, 2.5)
  
  g <- ggplotGrob(
         ggplot(df2, aes(x = "", y = !!y_var, 
                group = !!grp_var, fill = !!grp_var))+
         geom_bar(stat = 'identity', width = 1, color = '#696969')+
         scale_fill_manual(values = regsize_cols)+
         coord_polar(theta = "y", start = 0)+
         theme_void()+
         theme(legend.position = 'none',
               plot.background = element_blank(),
               panel.background = element_blank(),
               panel.border = element_blank(),
               axis.line = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid = element_blank(),
               )
    )
  p <- set_panel_size(p = NULL, g = g, 
                 margin = unit(0, "mm"), 
                 width = unit(pie_exp, "cm"), 
                 height = unit(pie_exp, "cm"))
  }

#----- Create veg map legend -----
veg_leg <- tm_shape(park_veg)+
  tm_fill(col = "fills")+
  tm_add_legend(title = "Habitat types", type = 'fill', 
                labels = park_veg$labels,
                col = park_veg$fills,
                border.col = "white",
                border.lwd = 3,
                size = 0.5)+
  tm_layout(legend.only = TRUE, 
            legend.text.size = 1,
            legend.title.size = 1.2)



