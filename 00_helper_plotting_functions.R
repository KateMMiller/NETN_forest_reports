#-----------------------
# Functions to help with plotting
#-----------------------

library(tidyverse)
library(egg)

#----- Set up pie charts -----
# Function to create formatted color list for plotting/legend
prep_sym_cols <- function(df, grp_var){
  map_colors_df <- df[df$group == grp_var, c('values','fills')]
  map_colors1 <- t(map_colors_df)
  colnames(map_colors1) <- map_colors1[1, ]
  map_colors <- map_colors1[-1, ]
  return(map_colors)
}

# Function to create symbol list for plotting/legend
prep_sym_shapes <- function(df, grp_var){
  map_shapes_df <- df[df$group == grp_var, c('values','shapes')]
  map_shapes1 <- t(map_shapes_df)
  colnames(map_shapes1) <- map_shapes1[1, ]
  map_shapes <- map_shapes1[-1, ]
  return(map_shapes)
}

# Function to create ggplot pie charts
pie_regsize_fun <- function(df, plotname, y_var, grp_var, std_var){
  grp_var <- enquo(grp_var)
  y_var <- enquo(y_var)
  
  df2 <- df[df$Plot_Name == plotname,] %>% ungroup()
  # pie_exp1 <- df2 %>% select(!!std_var) %>% unique() %>% as.numeric()
  # pie_r <- pie_min + (pie_max - pie_min)*pie_exp1 # expansion factors for pies range(0.7, 2.5)
  
  g <- ggplotGrob(
    suppressMessages(ggplot(df2, aes(x = "", y = !!y_var))+
      # {if(all(df2[, std_var] == 0)) geom_point(shape = 17, color = "white",
      #                                          size = 3.1)}+ #halo around symbol
        
      #{if(all(df2[, std_var] == 0)) geom_point(shape = 24, fill = "#ff7f00", size = 3)}+
      
      {if(any(df2[, std_var] > 0)) geom_bar(stat = 'identity', width = 1, color = 'white',
                                            alpha = 0, lwd = 0.6)}+ #adds white halo around pie

      {if(any(df2[, std_var] > 0)) geom_bar(aes(group = !!grp_var, fill = !!grp_var), 
                                            stat = 'identity', width = 1, color = '#5A5A5A', lwd = 0.4)}+
        
      {if(any(df2[, std_var] >0)) coord_polar(theta = "y", start = 0)}+
      
      scale_fill_manual(values = regsize_cols)+
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
  ) #end of suppressWarnings
  
  g
  # p <- set_panel_size(p = NULL, g = g, 
  #                     margin = unit(0, "mm"), 
  #                     width = unit(pie_min, "cm"), 
  #                     height = unit(pie_min, "cm"))
}