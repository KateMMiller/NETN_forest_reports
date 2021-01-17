#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#----------------------
#
# x and y are quoted coordinates, df is the dataframe to turn into simple feature
# Using CRS 5070 (UTM albers). Returns a data.frame with nudged coordinates
#   - consider adding crs as an argument, but keep it in UTM
#
nudge_XY_sing <- function(df, x, y){
  
  df$X1 <- df[,x]
  df$Y1 <- df[,y]
  # create sf from df
  
  sf <- st_as_sf(df, coords = c("X1", "Y1"), crs = 5070, agr = "constant")

  # Approximate pie size for each plot
  sf <- sf %>% mutate(fig_radius = (totreg_std2 + 1.5*sqrt(totreg_std2))*100)
  
  # Calculate the distance between the closest points. Take only the closest point
  # st_distance returns an array. Have to do a lot of munging to get the wanted format
  df_dist <- st_distance(sf) %>% data.frame() %>% set_names(plot_list) %>% #distance b/t points
    mutate(Plot_Name = plot_list) %>% 
    select(Plot_Name, everything()) %>% 
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
                                tot_radius = fig_radius + fig_radius2) %>% 
                         select(-dir, -fig_radius)
  
  names(df_geom)
  names(sf)
  df_geom_rad <- left_join(df_geom, st_drop_geometry(sf), by = "Plot_Name")
  names(df_geom_rad)
  
  # Convert final output to sf based on original plot coords
  
  sf_geom_rad <- st_as_sf(df_geom_rad, coords = c("X1", "Y1"), crs = 5070, agr = "constant")
  
  # # Create buffer to be similar to the size of pies. Use for checking plot overlap
  #sf_buff <- st_buffer(sf_geom_rad, sf_geom_rad$fig_radius)
  
  #plots_to_shift <- check_overlap(sf_buff)
  plots_to_shift <- check_overlap(sf_geom_rad)
  
  #sf_geom_rad2 <- st_as_sf(df_geom_rad, coords = c("X1", "Y1"), crs = 5070, agr = "constant")
  
  # df_geom_rad <- df_geom_rad %>% mutate(shift = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
  #                                                      0.6*((fig_radius + fig_radius2) - dist), 0), #shift slightly more than dist 
  #                                       X_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
  #                                                        X1 + dir_x*(sin(angle*pi/180))*shift, X1),
  #                                       Y_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
  #                                                        Y1 + dir_y*(cos(angle*pi/180))*shift, Y1)) %>% 
  #                                select(Plot_Name, X1, Y1, X_nudge, Y_nudge, Unit_Code:totreg_std2, fig_radius) %>% 
  #                                rename(X_orig = X1, Y_orig = Y1)
  
  ran_angle <- runif(1, -10, 10)
  df_geom_rad <- df_geom_rad %>% mutate(shift = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                       0.8*((fig_radius + fig_radius2) - dist), 0), #shift slightly more than dist 
                                        X_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         X1 + dir_x*(sin((ran_angle+angle)*pi/180))*shift, X1),
                                        Y_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         Y1 + dir_y*(cos((ran_angle+angle)*pi/180))*shift, Y1)) %>% 
    select(Plot_Name, X1, Y1, X_nudge, Y_nudge, Unit_Code:totreg_std2, fig_radius) %>% 
    rename(X_orig = X1, Y_orig = Y1)
  #df_check <- df_geom_rad[,c(1:5,29:30,6:15,27,28)]
  
  sf_final <- st_as_sf(df_geom_rad, coords = c("X_nudge", "Y_nudge"), crs = 5070) 
  #sf_final_buff <- st_buffer(sf_final, sf_final$fig_radius)
  
  return(sf_final)
  }
