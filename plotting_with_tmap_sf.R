#-----------------------
# Mapping with tmap
#-----------------------
library(tmap)
library(sf)
#library(rworldmap) # for mapPies
library(tidyverse)
library(forestNETN)
library(geosphere) # for destPoint to calc nudge coords


#----- Set up park controls -----
park_code <- 'MORR'
park_long_name = "Morristown National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}


#----- Load spatial data -----
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
                        totreg_std2 = ifelse(totreg_std < 0.2, 0.2, totreg_std)
)
head(map_controls)

# Create long list for ggplot pie chart list
reg2$Plot_Name[reg2$totreg_m2 > 0]

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

# Create list of pie charts by plot_list
pie_list <- map(plot_list, ~pie_fun(reg_long, .x, y_var = dens, grp_var = size_class)) %>% 
            set_names(plot_list)
    #pie_list$`MORR-002`$data$dens #all 0s

#----- Check and fix pie overlap -----
# Convert forest data to simple feature
reg2 <- reg2 %>% mutate(plot_num = as.numeric(Plot_Number), 
                        fig_radius = totreg_std2*200)
reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = 26918, agr = "constant")
st_crs(reg_sf) # UTM Zone 1#N
reg_sf_alb <- st_transform(reg_sf, crs = 5070)
st_crs(reg_sf_alb) # Conus Albers Equal Area

# check_overlap <- function(sf, row){
#   # Create buffer to be similar to the size of pies. Use for checking plot overlap
#   sf_buff <- st_buffer(sf, sf$fig_radius)
#   overlaps <- st_overlaps(sf_buff)  %>% set_names(sf_buff$Plot_Name)
#   
#   df <- data.frame(names(overlaps[row]), length(overlaps[[row]]))
#   colnames(df) <- c("Plot_Name", "num_overlaps")
#   return(df)
# }

# ID plots that have at least one overlapping pie. Used within nudge_XY
check_overlap <- function(sf){
  # Create buffer approximately the size of pies. 
  sf_buff <- st_buffer(sf, sf$fig_radius)
  
  # Determine pies that overlap
  overlaps <- st_drop_geometry(st_intersection(sf_buff)) %>% group_by(Plot_Name) %>% 
    summarize(num_overlaps = sum(!is.na(Plot_Name))-1, 
              .groups = "drop") %>% ungroup() %>% 
    filter(num_overlaps > 0) %>% select(Plot_Name) %>% mutate(type = 'overlap')
  
  # Determine pies that are within another pie
  withins <- st_within(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(inside = length(x)-1)) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(inside > 0) %>% select(Plot_Name) %>% mutate(type = 'within')
  
  # Bind plot list of pies to shift coords
  plots_to_shift <- rbind(overlaps,withins) %>% arrange(Plot_Name, type) %>% 
    select(Plot_Name) %>% unique()
  
  return(plots_to_shift)
}

nudge_XY <- function(sf){
  # Approximate pie size for each plot
  sf$fig_radius <- sf$totreg_std2*200
  
  # Calculate the distance between the closest points. Take only the closest point
    # st_distance returns an array. Have to do a lot of munging to get the wanted format
  df_dist <- st_distance(sf) %>% data.frame() %>% set_names(plot_list) %>% #distance b/t points
               mutate(Plot_Name = plot_list) %>% select(Plot_Name, everything()) %>% 
               pivot_longer(cols = c(-Plot_Name), names_to = "closest_plot", values_to = 'dist') %>% 
               filter(Plot_Name != closest_plot) %>% #remove plot pairs that are 0
               group_by(Plot_Name) %>% arrange(Plot_Name, dist) %>% slice(1) %>% # slice the 1 closest point
               ungroup() 
  
  # Set up coordinates for each plot
  df_c1 <- data.frame(cbind(sf$Plot_Name, df_dist$dist, sf$fig_radius, st_coordinates(sf))) %>% 
    set_names(c("Plot_Name", "dist", "fig_radius", "X1", "Y1"))
  
  # Set up coordinates for each plot's closest neighbor
  df_c2 <- left_join(df_dist[,c("Plot_Name", "closest_plot")], df_c1[,c("Plot_Name", "X1", "Y1", "fig_radius")], 
                     by = c("closest_plot" = "Plot_Name")) %>% 
    set_names(c("Plot_Name", "closest_plot", "X2", "Y2", "fig_radius2"))
  
  # Join coord dfs, to have plot coords and closest neighbor's coords. Convert to numeric
  df_geom <- full_join(df_c1, df_c2, by = "Plot_Name") %>% select(Plot_Name, closest_plot, everything()) %>% 
             mutate(across(c(dist, fig_radius, X1, X2, Y1, Y2, fig_radius2), as.numeric)) # all cols were chars
  
  # Add nudge angles for each closet pair
  df_geom <- df_geom %>% mutate(diff_x = X1 - X2,
                                diff_y = Y1 - Y2,
                                dir_x = ifelse(diff_x > 0, 1, -1),
                                dir_y = ifelse(diff_y > 0, 1, -1),
                                angle = acos(abs(diff_x)/dist)*(180/pi),
                                dir = dir_x + dir_y,
                                # angle_shift = case_when(dir == 2 ~ 90 - angle,
                                #                         dir == -2 ~ 180 + angle,
                                #                         abs(dir) < 2 & dir_x == -1 ~ 360 - angle,
                                #                         abs(dir) < 2 & dir_y == -1 ~ 90 + angle),
                                tot_radius = fig_radius + fig_radius2) %>% 
                         select(-dir, -fig_radius)
  
  df_geom_rad <- left_join(df_geom, st_drop_geometry(sf), by = "Plot_Name")
  
  # Convert final output to sf based on original plot coords
  sf_geom_rad <- st_as_sf(df_geom_rad, coords = c("X1", "Y1"), crs = 5070, agr = "constant")

  # # Create buffer to be similar to the size of pies. Use for checking plot overlap
  sf_buff <- st_buffer(sf_geom_rad, sf_geom_rad$fig_radius)
  
  plots_to_shift <- check_overlap(sf_buff)

  sf_geom_rad2 <- st_as_sf(df_geom_rad, coords = c("X1", "Y1"), crs = 5070, agr = "constant")
  
  df_geom_rad <- df_geom_rad %>% mutate(shift = ifelse((fig_radius + fig_radius2) - dist > 0,
                                                       (fig_radius + fig_radius2) - dist, 0), 
                                        X_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         X1 + dir_x*(sin(angle*pi/180))*shift, X1),
                                        Y_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         Y1 + dir_x*(cos(angle*pi/180))*shift, Y1)
                                        )
  #df_check <- df_geom_rad[,c(1:5,29:30,6:15,27,28)]
  
  sf_test <- st_as_sf(df_geom_rad, coords = c("X_nudge", "Y_nudge"), crs = 5070)
  
  check_overlap(sf_test)

  }



# test <- st_within(sf_buff)
 test2 <- st_intersection(sf_buff)
 str(test2)
# names(test2)

ol_plots <- bind_rows(lapply(seq_along(1:nrow(reg_sf_alb)), 
                             function(x){check <- check_overlap(reg_sf_alb, x)})) 

reg_sf_alb_dist <- st_distance(reg_sf_alb) %>% unlist() %>% data.frame() %>% set_names(plot_list)
reg_sf_alb2 <- cbind(reg_sf_alb$Plot_Name, reg_sf_alb_dist, ol_plots) %>% arrange(-num_overlaps) %>% select(-1)
head(reg_sf_alb2)

reg_sf_long <- reg_sf_alb2 %>% pivot_longer(cols = c(-Plot_Name, -num_overlaps),
                                            names_to = "plot_comp", values_to = "distance") %>% 
                                            arrange(-num_overlaps, -distance) %>% 
                               filter(Plot_Name != plot_comp)
names(reg_sf_long)

ref_sf_long2 <- reg_sf_long %>% filter(num_overlaps > 0) %>% arrange(Plot_Name, distance) 

ref_sf_long2

ref_sf_long3 <- left_join(ref_sf_long2, st_drop_geometry(sf_buff[, c("Plot_Name", "fig_radius")]), 
                          by = "Plot_Name")
ref_sf_long3

# Function used to check overlap within nudge_XY. The sf is the point feature with X/Y coordinates
# The sf must also have a fig_radius field
check_overlap <- function(sf, row){
  # Create buffer to be similar to the size of pies. Use for checking plot overlap
  sf_buff <- st_buffer(sf, sf$fig_radius)
  overlaps <- st_overlaps(sf_buff)  %>% set_names(sf_buff$Plot_Name)
  df <- data.frame(names(overlaps[row]), length(overlaps[[row]]))
  colnames(df) <- c("Plot_Name", "num_overlaps")
  return(df)
}

sf = reg_sf_alb

# Function to iteratively nudge X Y coordinates until their graphs won't overlap
# Must have a fig_radius column in sf_origin
#nudge_XY <- function(sf){
    # Check for overlaps
    ol_plots <- bind_rows(lapply(seq_along(1:nrow(sf)), 
                             function(x){check <- check_overlap(sf, x)})) 
    
    ol_plots2 <- cbind(st_drop_geometry(sf), ol_plots, st_coordinates(sf))
    
    if(sum(ol_plots2$num_overlaps) == 0){
      cat("There are no overlapping plots. Graphs can be plotted using original coordinates.")
      break()
    } 
    
    if(sum(ol_plots2$num_overlaps) > 0) cat("There are", sum(ol_plots2$num_overlaps>0), "plots that overlap.",
                                            "Use X_Nudge and Y_Nudge coordinates to plot graphs.")
    
    table(ol_plots2$num_overlaps)
    # random sign to nudge plots
    rand_sign = sample(c(-1,1),1)
    ol_plots2$X_nudge <- ifelse(ol_plots2$num_overlaps > 0, 
                                ol_plots2$X + rand_sign * 0.5 * ol_plots2$pie_radius, ol_plots2$X)
    ol_plots2$Y_nudge <- ifelse(ol_plots2$num_overlaps > 0, 
                                ol_plots2$Y + rand_sign * 0.5 * ol_plots2$pie_radius, ol_plots2$Y)
    
    ol_plots3 <- subset(ol_plots2, select = -c(num_overlaps))
    
    ol_plots_check <- st_as_sf(ol_plots3, coords = c("X_nudge", "Y_nudge"), crs = 5070, agr = "constant")
    
    reg_buff2 <- st_buffer(ol_plots_check, ol_plots_check$fig_radius)
    
    ol_plots_check2 <- bind_rows(lapply(seq_along(1:nrow(reg_buff2)), 
                                        function(x){check <- check_overlap(reg_buff2, x)})) 
    table(ol_plots_check2$num_overlaps)

#}

test <- st_jitter(reg_buff, factor = 0.01)
plot(test[1])
plot(reg_buff[1])
#
if(sum(ol_plots2$num_overlap))

  test <- nudge_XY(reg_sf_alb, reg_buff)
test

  
test <- st_overlaps(reg_buff) %>% set_names(reg_buff$Plot_Name)
length(test[[1]])
df <- data.frame(names(test[1]), length(test[[1]]))
df

names(test)

# bbox <- st_bbox(reg_buff)
# scale_m <- data.frame(x_diff = bbox$xmax - bbox$xmin,
#                       y_diff = bbox$ymax - bbox$ymin)
# rownames(scale_m) <- NULL
# scale_m

ol_plots2$X_nudge <- ifelse(ol_plots2$num_overlaps > 0, ol_plots2$X + 0.5*ol_plots2$pie_radius, ol_plots2$X)
ol_plots2$Y_nudge <- ifelse(ol_plots2$num_overlaps > 0, ol_plots2$Y + 0.5*ol_plots2$pie_radius, ol_plots2$Y)
ol_plots2 <- subset(ol_plots2, select = -c(num_overlaps))

ol_plots_check <- st_as_sf(ol_plots2, coords = c("X_nudge", "Y_nudge"), crs = 5070, agr = "constant")
reg_buff2 <- st_buffer(ol_plots_check, ol_plots_check$pie_radius)

ol_plots_check2 <- bind_rows(lapply(seq_along(1:nrow(reg_buff2)), 
                             function(x){check <- check_overlap(reg_buff2, x)})) 




st_area(reg_buff)
point_dist <- t(as.data.frame(st_distance(reg_buff)))
head(point_dist)
colnames(point_dist) <- unique(reg_buff$Plot_Name)
str(point_dist)

point_dist$Plot_Name <- reg_buff$Plot_Name
str(point_dist)

dist_comb <- cbind(point_dist, ol_plots)
head(dist_comb)
# Still don't have exact geometry of pie charts with buffer. Need to work on that
# Then need to figure out how to take the plots that overlap, and nudge their
# geometries so they don't.

# Plot data
basemap <- tm_shape(park_veg) +
             tm_fill("fills") +
           tm_shape(park_bound) +
             tm_borders(col = 'black', lwd = 2)

pie_list

map2 <- basemap + tm_shape(reg_sf_alb) +
          tm_symbols(size = "totreg_std2", 
                     shape = "Plot_Name",
                     icon.scale = 4,
                     size.lim = c(0.001,4),
                     #size.max = 1, jitter = 0.5, xmod = 0.1, ymod = 0.1, scale = 1.2,
                     group = "Charts",
                     shapes = pie_list, 
                     grob.dim = c(width = 48, height = 48, 
                                  render.width = 256, render.height = 256),
                     border.col = NA, border.lwd = NA)+
          tm_legend(show = FALSE) + tm_compass(size = 2) + tm_scale_bar()

reg_buff <- st_buffer(reg_sf_alb, reg_sf_alb$totreg_std2*200)

map2 + tm_shape(reg_buff %>% filter(totreg_m2 > 0)) + 
         tm_borders(col = 'red') + tm_text("plot_num") + 
       tm_shape(reg_sf_alb %>% filter(totreg_m2 == 0))+
         tm_symbols(shape = 24, size = 0.4, col = "#ff7f00")

# reg_sf_alb <- reg_sf_alb %>% mutate(bub_size = totreg_std2)
# head(reg_sf_alb)

map2 + tm_shape(reg_sf_alb %>% filter(totreg_m2 > 0))+
       tm_bubbles(size =  "totreg_std2",
                  shape = 21,
                  #size.lim = c(0, 0.1),
                  scale = 2,
                  alpha = 0,
                  border.col = 'black')

?tm_bubbles
?tm_symbols
