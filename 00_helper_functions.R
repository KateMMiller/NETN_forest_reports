#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#   - There's still a check of overlap I'm missing- if a buffer is inside a plot only the inside plot is caught
#----------------------
# ID plots that have at least one overlapping pie. Used within nudge_XY
check_overlap <- function(sf){
  
  # Create buffer approximately the size of pies. 
  sf_buff <- st_buffer(sf, sf$fig_radius)
  plot_list <- sort(unique(sf$Plot_Name))
  
  # Determine pies that overlap
  overlaps1 <- st_drop_geometry(st_intersection(sf_buff)) %>% group_by(Plot_Name) %>%
    summarize(num_overlaps = sum(!is.na(Plot_Name))-1,
              .groups = "drop") %>% ungroup() %>%
    filter(num_overlaps > 0) %>% select(Plot_Name) %>% mutate(type = 'overlap1')
  
  overlaps2 <- st_overlaps(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(overs = length(x))) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(overs > 0) %>% select(Plot_Name) %>% mutate(type = 'overlap2')
  
  # Determine pies that are within another pie
  withins <- st_within(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(inside = length(x)-1)) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(inside > 0) %>% select(Plot_Name) %>% mutate(type = 'within')
  
  covers <- st_covers(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(covers = length(x)-1)) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(covers > 0) %>% select(Plot_Name) %>% mutate(type = 'covers')
  
  # Bind plot list of pies to shift coords
  plots_to_shift <- rbind(overlaps1, overlaps2, withins, covers) %>% arrange(Plot_Name, type) %>% 
    select(Plot_Name) %>% unique()
  
  return(plots_to_shift)
}

#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal helper function used within nudge_XY 
#----------------------
check_overlap_text <- function(df, plotname, CRS){
  min_fig_rad <- min(df$fig_radius, na.rm = T) + 
    0.1*(min(df$fig_radius, na.rm = T))
  
  # Set up pies/labels to check against
  df_pies <- df %>% mutate(type = "pie", X = X_nudge, Y = Y_nudge)
  df_text <- df %>% mutate(type = 'text', X = X_text, Y = Y_text,
                           fig_radius = min(fig_radius))#+ 0.1*(min(fig_radius))) #added 10% buffer to size 
  df_text_rest <- df_text %>% filter(!Plot_Name %in% plotname)
  df_pies_rest <- df_pies %>% filter(!Plot_Name %in% plotname)
  
  colnames <- c("Plot_Name", "X", "Y", "fig_radius")
  sf_comb <- st_as_sf(rbind(df_pies_rest[,colnames], df_text_rest[,colnames]),
                      coords = c('X', 'Y'), crs = CRS)
  sf_buff <- st_buffer(sf_comb, dist = sf_comb$fig_radius) %>% st_union()
  
  # Set up label for plotname being checked
  df_pies_plot <- df_pies %>% filter(Plot_Name %in% plotname)
  df_text_plot <- df_text %>% filter(Plot_Name %in% plotname)
  sf_plot <- st_as_sf(df_text_plot, coords = c("X", "Y"), crs = CRS)
  sf_plot_buff <- st_buffer(sf_plot, dist = sf_plot$fig_radius)
  
  check_over <- st_intersects(sf_buff, sf_plot_buff, sparse = F, 
                              dist = min_fig_rad^2) # adds buffer
}


nudge_text <- function(df, plotname, quiet = TRUE){
  
  #df2 <- df[df$Plot_Name == plotname,]
  orig_df <- df %>% rename(X_text_orig = X_text, 
                           Y_text_orig = Y_text) 
  
  #set while loop parameters
  text_over <- TRUE 
  label_placement <- 1
  loc = "BR"
  
  # Placement properties
  place_props <- data.frame(order = 1:9,
                            x = c(1, -1, -1, 1, 0, -1, 0, 1, 1), 
                            y = c(-1, -1, 1, 1, -1, 0, 1, 0, -1),
                            loc = c("BR", "BL", "UL", "UR", "BC", "ML", "UC", "MR", "BR")) #8th puts back to beginning
  
  while(text_over == TRUE & label_placement < 10){
    
    df_text_nudge <- df
    
    # iterate through placement properties
    new_place <- place_props[label_placement, ] # skips first BR
    x_sign = new_place$x
    y_sign = new_place$y
    loc = new_place$loc
    
    
    df_text_nudge$X_text[df_text_nudge$Plot_Name == plotname] <- 
      ifelse(df_text_nudge$std_var[df_text_nudge$Plot_Name == plotname] == 0, 
               df_text_nudge$X_nudge[df_text_nudge$Plot_Name == plotname] + 5,
             df_text_nudge$X_nudge[df_text_nudge$Plot_Name == plotname] + 
               df_text_nudge$fig_radius[df_text_nudge$Plot_Name == plotname] * x_sign
             )
    
    df_text_nudge$Y_text[df_text_nudge$Plot_Name == plotname] <- 
      ifelse(df_text_nudge$std_var[df_text_nudge$Plot_Name == plotname] == 0, 
               df_text_nudge$Y_nudge[df_text_nudge$Plot_Name == plotname] -
                 1.2*df_text_nudge$fig_radius[df_text_nudge$Plot_Name == plotname], #1.2 to give more space b/t points
             df_text_nudge$Y_nudge[df_text_nudge$Plot_Name == plotname] + 
               df_text_nudge$fig_radius[df_text_nudge$Plot_Name == plotname] * y_sign)
    
    # Check if the text overlaps with anything
    text_over <- check_overlap_text(df_text_nudge, plotname)
    
    out_row <- df_text_nudge[df_text_nudge$Plot_Name == plotname, c("Plot_Name", "X_text", "Y_text")]
    out_row$text_over <- text_over
    out_row$label_placement <- label_placement
    out_row$label_loc <- loc
    out_row$x_sign <- x_sign
    out_row$y_sign <- y_sign

    df_comb <- rbind(out_row)
      
    if(quiet == FALSE){
      if(text_over == TRUE){
        if(label_placement == 9){cat("No non-overlapping label placement for ", 
                                     plotname, ". Reset to default BR corner.", "\n", sep = "")} 
      } else if(text_over == FALSE){cat("Non-overlapping position for ", plotname, " is ", loc, ".", "\n", sep = "")}
    }

    label_placement <- label_placement + 1
    
    df <- df_text_nudge
    
  } # end of while

  df_text_final <- df_comb %>% filter(text_over == FALSE | label_placement == 9) #%>% 
                              # select(-text_over, -label_placement)
  return(df_text_final)
} # end of function


#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#----------------------
#
# x and y are quoted coordinates, df is the dataframe to turn into simple feature
# Using CRS 5070 (UTM albers). Returns a data.frame with nudged coordinates
#   - consider adding crs as an argument, but keep it in UTM
#
nudge_XY_sing <- function(df, x, y, runs, stdvar, min_shift = 0.5, CRS){
  if(!exists('runs')){runs = 1}
  
  df$X1 <- df[,x]
  df$Y1 <- df[,y]
  df$std_var <- df[, stdvar]
  # create sf from df
  plot_list <- sort(unique(df$Plot_Name))
  
  sf <- st_as_sf(df, coords = c("X1", "Y1"), crs = CRS, agr = "constant")
  
  # Approximate pie size for each plot
  
  # Calculate the distance between the closest points. Take only the closest point
  # st_distance returns an array. Have to do a lot of munging to get the wanted format
  
  slice_num <- ifelse(runs > 20, sample(c(1, 2), 1, replace = TRUE), 1)
  
  df_dist <- st_distance(sf) %>% data.frame() %>% set_names(plot_list) %>% #distance b/t points
    mutate(Plot_Name = plot_list) %>% 
    select(Plot_Name, everything()) %>% 
    pivot_longer(cols = c(-Plot_Name), names_to = "closest_plot", values_to = 'dist') %>% 
    filter(Plot_Name != closest_plot) %>% #remove plot pairs that are 0
    group_by(Plot_Name) %>% arrange(Plot_Name, dist) %>% slice(slice_num) %>% # slice the 1 or 2 closest point
    ungroup() 
  
  # Set up coordinates for each plot
  df_c1 <- data.frame(cbind(sf$Plot_Name, df_dist$dist, sf$fig_radius, st_coordinates(sf))) %>% 
    set_names(c("Plot_Name", "dist", "fig_radius", "X1", "Y1"))
  
  # Set up coordinates for each plot's closest neighbor
  df_c2 <- left_join(df_dist[,c("Plot_Name", "closest_plot")], df_c1[,c("Plot_Name", "X1", "Y1", "fig_radius")], 
                     by = c("closest_plot" = "Plot_Name")) %>% 
    set_names(c("Plot_Name", "closest_plot", "X2", "Y2", "fig_radius2"))
  
  # Join coord dfs, to have plot coords and closest neighbor's coords. Convert to numeric
  df_geom <- full_join(df_c1, df_c2, by = "Plot_Name") %>% 
    select(Plot_Name, closest_plot, everything()) %>% 
    mutate(across(c(dist, fig_radius, X1, X2, Y1, Y2, fig_radius2), as.numeric)) # all cols were chars
  
  # Add nudge angles for each closet pair
  df_geom <- df_geom %>% mutate(diff_x = X1 - X2,
                                diff_y = Y1 - Y2,
                                dir_x = ifelse(diff_x > 0, 1, -1), 
                                dir_y = ifelse(diff_y > 0, 1, -1), 
                                angle = asin(abs(diff_x)/dist)*(180/pi),
                                tot_radius = fig_radius + fig_radius2) %>% 
    select(-fig_radius)
  
  df_geom_rad <- left_join(df_geom, st_drop_geometry(sf), by = "Plot_Name")
  
  # Convert final output to sf based on original plot coords
  sf_geom_rad <- st_as_sf(df_geom_rad, coords = c("X1", "Y1"), crs = CRS, agr = "constant")
  
  # # Create buffer to be similar to the size of pies. Use for checking plot overlap
  #sf_buff <- st_buffer(sf_geom_rad, sf_geom_rad$fig_radius)
  
  plots_to_shift <- check_overlap(sf_geom_rad)
  
  ran_angle <- ifelse(runs > 10,
                      sample(c(rep(0,4), -45, 45), 1),
                      0) # add random noise if pie gets stuck after 10 runs, but still bias towards 0
  
  inc_dist <- if(runs > 20){0.8
  } else if(runs < 20 && runs > 10){0.6
  } else{min_shift} # increase shift distance for longer runs of pies get stuck
  
  df_geom_rad <- df_geom_rad %>% mutate(shift = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                       inc_dist*((fig_radius + fig_radius2) - dist), 0), 
                                        X_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         X1 + dir_x*sin((ran_angle+angle)*(pi/180))*shift, X1),
                                        Y_nudge = ifelse(Plot_Name %in% plots_to_shift$Plot_Name,
                                                         Y1 + dir_y*cos((ran_angle+angle)*(pi/180))*shift, Y1),
                                        X_text = ifelse(std_var == 0, X_nudge, # better centering under point
                                                                      X_nudge + fig_radius),
                                        Y_text = Y_nudge - fig_radius) %>% 
    select(Plot_Name, X1, Y1, X_nudge, Y_nudge, X_text, Y_text, Unit_Code:std_var, fig_radius) %>% 
    rename(X_orig = X1, Y_orig = Y1)
  
  sf_nudge <- st_as_sf(df_geom_rad, coords = c("X_nudge", "Y_nudge"), crs = CRS) 
  
  return(sf_nudge)
  
}

#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#----------------------
# Calls and returns a data.frame with nudged coordinates
# Args x and y are quoted coordinates, df is the dataframe to turn into simple feature
# Using CRS 5070 (UTM albers). 
#   - consider adding crs as an argument, but keep it in UTM
#   - add quietly = T/F so you can turn the chatter on/off in console
#

nudge_XY <- function(df, x, y, stdvar, min_shift = 0.5, max_iter = 10, CRS, 
                     nudge_point = TRUE, nudge_text = TRUE, quiet = TRUE){
  # source("nudge_XY_sing.R")
  # source("check_overlap.R")
  runs <- 0
  num_overlap <- 1
  orig_df_XY <- df[ , c("Plot_Name", x, y)] %>% set_names("Plot_Name", "X_orig", "Y_orig") 
  
  plot_list <- sort(unique(df$Plot_Name))
  
  # Add if nudge_point == TRUE{ } for option not to run this part
  while(num_overlap > 0 && runs < max_iter){
    sf_nudge <- nudge_XY_sing(df, x, y, runs = runs, stdvar, min_shift)
    
    num_overlap <- nrow(check_overlap(sf_nudge))
    overlaps <- c(check_overlap(sf_nudge))
    runs <- runs + 1
    
    if(quiet == FALSE){
    if(num_overlap > 0 && runs == max_iter){
      cat("After", paste0(runs), "iterations,", as.character(num_overlap), "graphs are still overlapping: ",
          as.character(overlaps), "\n",
          "Either increase max_iter or manually shift overlapping charts.", "\n")
    } else if (num_overlap > 0 && runs < max_iter){
      cat("Number of overlapping plots after", as.character(runs), "iterations: ", 
          as.character(num_overlap), "\n")
    }
    if(num_overlap == 0){
      cat("Pies are ready to plot using X_Nudge and Y_Nudge after ", as.character(runs), 
          " iterations.")
    }
    }
    
    df <- cbind(st_drop_geometry(sf_nudge), st_coordinates(sf_nudge)) %>% 
      set_names(names(st_drop_geometry(sf_nudge)), "X_nudge", "Y_nudge")
    x = "X_nudge"
    y = "Y_nudge"
    
  }
  
  df_nudge <- cbind(st_drop_geometry(sf_nudge), st_coordinates(sf_nudge)) %>% 
    set_names(names(st_drop_geometry(sf_nudge)), "X_nudge", "Y_nudge") %>% 
    select(-X_orig, -Y_orig)
  
  # Add if(nudge_text == TRUE){} for option not to run this part
  # Text label nudges next
  df_nudge_text <- purrr::map_df(plot_list, ~nudge_text(df_nudge, plotname = .x, quiet = quiet))

  # Prepare final dataset
  # merge original to nudged pie dataset
  df_comb <- merge(orig_df_XY, df_nudge, by = "Plot_Name", all.x = TRUE, all.y = TRUE) %>% 
    select(-X, -Y, -X_text, -Y_text) 

  # merge df_comb to nudged text dataset
  
  df_final <- merge(df_comb, df_nudge_text, by = "Plot_Name", all.x = TRUE, all.y = TRUE)
  
  return(df_final)
}


