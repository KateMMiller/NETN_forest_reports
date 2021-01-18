#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#----------------------
#
# x and y are quoted coordinates, df is the dataframe to turn into simple feature
# Using CRS 5070 (UTM albers). Returns a data.frame with nudged coordinates
#   - consider adding crs as an argument, but keep it in UTM
#   - consider inputting sf and outputting sf instead of df to sf to df, or make it an argument
#   - add quietly = T/F so you can turn the chatter on/off in console
#
nudge_XY <- function(df, x, y, stdvar, max_iter = 5){
  runs <- 0
  num_overlap <- 1
  orig_df_XY <- df[ , c("Plot_Name", x, y)] %>% set_names("Plot_Name", "X_orig", "Y_orig")
  
  
  while(num_overlap > 0 && runs < max_iter){
    sf_nudge <- nudge_XY_sing(df, x, y, runs = runs, stdvar)
  
    num_overlap <- nrow(check_overlap(sf_nudge))
    overlaps <- c(check_overlap(sf_nudge))
    runs <- runs + 1
    
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
    
    df <- cbind(st_drop_geometry(sf_nudge), st_coordinates(sf_nudge)) %>% 
                set_names(names(st_drop_geometry(sf_nudge)), "X_nudge", "Y_nudge")
    x = "X_nudge"
    y = "Y_nudge"
    
  }
  
  df_nudge <- cbind(st_drop_geometry(sf_nudge), st_coordinates(sf_nudge)) %>% 
                set_names(names(st_drop_geometry(sf_nudge)), "X_nudge", "Y_nudge") %>% 
                select(-X_orig, -Y_orig)
  
  df_final <- merge(orig_df_XY, df_nudge, all.x = TRUE, all.y = TRUE)
  
  return(df_final)
  }



