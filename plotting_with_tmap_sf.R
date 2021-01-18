#-----------------------
# Mapping with tmap
#-----------------------
library(tmap)
library(sf)
library(tidyverse)
library(forestNETN)
library(egg) # for set_panel_size; expose ggplot layout

source("nudge_XY.R")
source("nudge_XY_sing.R")
source("check_overlap.R")
#----- Next Steps -----
# 1. Set up markdown park templates, so MIMA and ROVA can be 2 layouts and ACAD can be 3
# 2. Add none symbol to pie_list for 0s
# 3. Set up for ACAD
# 4. Set up GIS layers for MIDN parks

#----- Set up park controls -----
park_code <- 'SARA'
#park_long_name = "Saratoga National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}


#----- Load spatial data -----
bounds <- st_read("./shapefiles/NETN_park_bounds_albers.shp")
#st_crs(bounds) #5070
vegmap <- st_read("./shapefiles/NETN_vegmap_simplified_albers.shp")
#st_crs(vegmap) #5070
plots <- st_read("./shapefiles/NETN_forest_plots_albers.shp")
#st_crs(plots) #5070

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

veg_colors <- read.csv("./shapefiles/Vegmap_colors.csv")
head(veg_colors)

# baselayers
park_bound <- bounds[bounds$Park == park_code, ]
park_veg <- vegmap[vegmap$Park == park_code, ]
park_plots <- plots[plots$Unit_Cd == park_code, ]

plot(park_bound)
plot(park_veg)
plot(park_plots)
head(map_controls)

intersect(names(park_veg), names(veg_colors))

park_veg <- left_join(park_veg, veg_colors, by = "veg_type") # merge shapefile and colors for each veg_type

#----- Prep forest data -----
importData()

reg <- joinRegenData(park = park_code, speciesType = "native", canopyForm = 'all', 
                     from = 2016, to = 2019)

reg2 <- reg %>% group_by(Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord) %>% 
  summarise(sd15_30 = sum(seed15.30, na.rm = TRUE), 
            sd30_100 = sum(seed30.100, na.rm = TRUE), 
            sd100_150 = sum(seed100.150, na.rm = TRUE), 
            sd150p = sum(seed150p, na.rm = TRUE),
            sap = sum(sap.den, na.rm = TRUE),
            totreg_m2 = (sd15_30 + sd30_100 + sd100_150 + sd150p +sap)/10000, 
            .groups = 'keep') %>% ungroup()

min_totreg <- min(reg2$totreg_m2)
diff_totreg <- diff(range(reg2$totreg_m2))

reg2 <- reg2 %>% mutate(totreg_std = (reg2$totreg_m2 - min_totreg) / (diff_totreg),
                        totreg_std2 = ifelse(totreg_std < 0.1, 0.1, totreg_std),
                        pie_exp = totreg_std2 + 2*sqrt(totreg_std2)
)

head(map_controls)

# Create long list for ggplot pie chart list
#reg2$Plot_Name[reg2$totreg_m2 > 0]

plot_list <- sort(unique(reg2$Plot_Name))

reg_long <- reg2 %>% select(Plot_Name, sd15_30:sap) %>% 
            pivot_longer(cols = -Plot_Name, 
                         names_to = "size_class", 
                         values_to = "dens") %>% 
            group_by(Plot_Name) %>% 
            mutate(totdens = sum(dens))

reg_long$size_class <- factor(reg_long$size_class,
                              levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))

reg_long <- reg_long %>% arrange(Plot_Name, size_class) 
intersect(names(reg2), names(reg_long))
reg_long2 <- left_join(reg_long, reg2, by = "Plot_Name") 

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
  pie_exp <- pie_exp1 + 2*sqrt(pie_exp1)
  
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

# Create list of pie charts by plot_list

pie_list <- map(plot_list, ~pie_fun(reg_long2, .x, y_var = dens, 
                                    grp_var = size_class, std_var = "totreg_std2")) %>% 
            set_names(plot_list)

#----- Check and fix pie overlap -----
# Convert forest data to simple feature
CRS <- if(park_code %in% c('ACAD', 'MIMA')){26919} else {26918}
reg2 <- reg2 %>% mutate(plot_num = as.numeric(Plot_Number), fig_radius = pie_exp * 100)

reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = CRS, agr = "constant")
reg_sf_alb <- st_transform(reg_sf, crs = 5070)
#st_crs(reg_sf_alb) # Conus Albers Equal Area

reg_df <- cbind(st_drop_geometry(reg_sf_alb), st_coordinates(reg_sf_alb))
reg_sf <- st_as_sf(reg_df, coords = c("X", "Y"), crs = 5070)
reg_sf <- st_buffer(reg_sf, reg_sf$fig_radius)
check_overlap(reg_sf)

plot(reg_sf[2])
test <- nudge_XY(reg_df, x = "X", y = "Y", stdvar = "totreg_std2", 30) #SAGA isn't working
test_sf <- st_as_sf(test, coords = c("X_nudge", "Y_nudge"), crs = 5070)
test_sf_buff <- st_buffer(test_sf, test_sf$fig_radius)

plot(reg_sf[2])
plot(test_sf_buff[1])
#----------------------

# Plot data
basemap <- tm_shape(park_veg) +
             tm_fill("fills") +
           tm_shape(park_bound) +
             tm_borders(col = 'black', lwd = 2)

#pie_list

map1 <- basemap + tm_shape(reg_sf_alb) +
  tm_symbols(#size = "totreg_std2", 
             shape = "Plot_Name",
             #icon.scale = 20,
             #size.lim = c(0.02,1),
             #size.max = 1, jitter = 0.5, xmod = 0.1, ymod = 0.1, scale = 1.2,
             group = "Charts",
             shapes = pie_list, 
             # grob.dim = c(width = 48, height = 48, 
             #              render.width = 256, render.height = 256),
             border.col = NA, border.lwd = NA)+
  tm_text("Plot_Number", size = 0.8)+
  tm_legend(show = FALSE) + tm_compass(size = 2) + tm_scale_bar()#+
  # tm_shape(st_buffer(reg_sf_alb, reg_sf_alb$fig_radius))+
  # tm_borders(col = 'red')

map1


map2 <- basemap + tm_shape(test_sf %>% arrange(-fig_radius)) +
          tm_symbols(#size = "totreg_std2", 
                     shape = "Plot_Name",
                     #icon.scale = 10,
                     #size.lim = c(0.01, 1), # relative to test_x_sf$totreg_std, which ranges 0,1
                     #size.max = 1, jitter = 0.5, xmod = 0.1, ymod = 0.1, scale = 1.2,
                     group = "Charts",
                     shapes = pie_list, 
                     grob.dim = c(width = 48, height = 48, 
                                  render.width = 256, render.height = 256),
                     border.col = NA, border.lwd = NA)+
          tm_text("Plot_Number", size = 0.8)+
          tm_legend(show = FALSE) + tm_compass(size = 2) + tm_scale_bar()#+
           # tm_shape(st_buffer(test_sf, test_sf$fig_radius))+
           # tm_borders(col = 'red')
map2

#tmap_arrange(map1, map2)
# landscape
#tmap_save(map1, width = 11, height = 8.5, units = 'in', dpi = 600, filename = paste0(park_code, "_regen.png"))
#tmap_save(map2, width = 11, height = 8.5, units = "in", dpi = 600, filename = paste0(park_code, "_regen_nudge30.png"))

#portrait
tmap_save(map1, width = 8.5, height = 11, units = 'in', dpi = 600, filename = paste0(park_code, "_regen.png"))
tmap_save(map2, width = 8.5, height = 11, units = "in", dpi = 600, filename = paste0(park_code, "_regen_nudge30.png"))


# 
# map2 + tm_shape(reg_sf_alb %>% filter(totreg_m2 > 0))+
#        tm_bubbles(size =  "totreg_std2",
#                   shape = 21,
#                   #size.lim = c(0, 0.1),
#                   scale = 2,
#                   alpha = 0,
#                   border.col = 'black')
# 
