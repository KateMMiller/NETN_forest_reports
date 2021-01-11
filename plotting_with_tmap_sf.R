#-----------------------
# Mapping with tmap
#-----------------------
library(tmap)
library(sf)
#library(rworldmap) # for mapPies
library(tidyverse)
library(forestNETN)

#----- Set up park controls
park_code <- 'MORR'
park_long_name = "Morristown National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}


#----- Load spatial data
bounds <- st_read("./shapefiles/NETN_park_bounds_albers.shp")
st_crs(bounds) #5070
vegmap <- st_read("./shapefiles/NETN_vegmap_simplified_albers.shp")
st_crs(vegmap) #5070
plots <- st_read("./shapefiles/NETN_forest_plots_albers.shp")
st_crs(plots) #5070

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

# Prep forest data
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
                        totreg_std2 = ifelse(totreg_std < 0.2, 0.2, totreg_std)
)
head(map_controls)

# Create long list for ggplot pie chart list
reg2$Plot_Name[reg2$totreg_m2 > 0]

plot_list <- sort(unique(reg2$Plot_Name[reg2$totreg_m2 > 0]))

reg_long <- reg2 %>% select(Plot_Name, sd15_30:sap) %>% 
            pivot_longer(cols = -Plot_Name, 
                         names_to = "size_class", 
                         values_to = "dens") %>% 
            group_by(Plot_Name) %>% 
            mutate(totdens = sum(dens))

reg_long$size_class <- factor(reg_long$size_class,
                              levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))

reg_long <- reg_long %>% arrange(Plot_Name, size_class)

# Function to create formatted color list
prep_sym_cols <- function(df, grp_var){
  map_colors_df <- df[df$group == grp_var, c('values','fills')]
  map_colors1 <- t(map_colors_df)
  colnames(map_colors1) <- map_colors1[1, ]
  map_colors <- map_colors1[-1, ]
  return(map_colors)
}

regsize_cols <- prep_sym_cols(map_controls, "regsize")

pie_fun <- function(df, plotname, y_var, grp_var){
  grp_var <- enquo(grp_var) 
  y_var <- enquo(y_var)
  df2 <- df[df$Plot_Name == plotname,]
  ggplotGrob(
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
  }

pie_list <- map(plot_list, ~pie_fun(reg_long, .x, y_var = dens, grp_var = size_class)) %>% 
            set_names(plot_list)

#pie_list$`MORR-002`$data$dens #all 0s

# Convert forest data to simple feature
reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = 26918, agr = "constant")
st_crs(reg_sf) # UTM Zone 1#N
reg_sf_alb <- st_transform(reg_sf, crs = 5070)
st_crs(reg_sf_alb) # Conus Albers Equal Area

head(reg_sf_alb)

basemap <- tm_shape(park_veg) +
             tm_fill("fills") +
           tm_shape(park_bound) +
             tm_borders(col = 'black', lwd = 2)

basemap + tm_shape(reg_sf_alb %>% filter(totreg_m2 >0)) +
          tm_symbols(size = "totreg_std2", 
                     shape = "Plot_Name",
                     shapes = pie_list, 
                     group = "Charts",
                     border.col = NA,
                     border.lwd = NA)+
  tm_legend(show = FALSE)

