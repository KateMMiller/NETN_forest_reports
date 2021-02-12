windowsFonts(A = windowsFont("Arial"))
park_code = "ROVA"
#park_code <- params$park_code

library(tidyverse)
library(forestNETN)
library(grid) # for parks with multiple units
library(gtable) # for extracting the legend from ggplot objects
library(gridtext) # for captions that wrap 

importData()

pie_int = 40 # ~Size of smallest pie in m. Used in 02_calculate_metric_functions to calc. fig_radius
# pie_int + pie_slope*0.01 will be the smallest pie size mapped
pie_slope = 200 # Expansion factor to increase pie size by (pie_int + pie_slope(std_var)). 
# Used in 02_calculate_metric_functions to calc. fig_radius

# Note: 00 scripts will eventually be added to a package
source("00_helper_functions.R") # nudge_XY and dependency functions
source("00_helper_plotting_functions.R") # pie, chart and corresponding legend functions
source("01_compile_base_layers.R") # load spatial data and set up metric-level templates/legends
source("02_calculate_metric_functions.R") # metric summary functions
source(paste0("03_park_template_", park_code, ".R")) # Creates park-specifi

regen_by_sizeclass(park_code, 2016, 2019, pie_int, pie_slope) # adds regsize_df & regsize_long to GE
# Nudge XY coords if pies will overlap
regsize_nudge <- nudge_XY(regsize_df, x = "X", y = "Y", stdvar = "totreg_std2", 
                          max_iter = 20, min_shift = 0.5, quiet = FALSE) 

# Create list of pies
vama_list <- sort(unique(regsize_nudge$Plot_Name[regsize_nudge$Unit == "VAMA"]))
rova_list <- sort(unique(regsize_nudge$Plot_Name[regsize_nudge$Unit != "VAMA"]))

vama_regsize_pies <- map(vama_list, 
                         ~pie_regsize_fun(regsize_long %>% filter(Unit == "VAMA"), .x,
                                          y_var = dens, grp_var = size_class, std_var = "totreg_std2"))
rova_regsize_pies <- map(rova_list, 
                         ~pie_regsize_fun(regsize_long %>% filter(Unit != "VAMA"), .x,
                                          y_var = dens, grp_var = size_class, std_var = "totreg_std2"))


rova_sf <- st_as_sf(regsize_nudge %>% filter(Unit != "VAMA"), 
                    coords = c("X_nudge", "Y_nudge"), crs = 5070) 

vama_sf <- st_as_sf(regsize_nudge %>% filter(Unit == "VAMA"), 
                    coords = c("X_nudge", "Y_nudge"), crs = 5070)

rova_text <- st_as_sf(regsize_nudge %>% filter(Unit != "VAMA"), 
                      coords = c("X_text", "Y_text"), crs = 5070)

vama_text <- st_as_sf(regsize_nudge %>% filter(Unit == "VAMA"),
                      coords = c("X_text", "Y_text"), crs = 5070)

rova_buff <- st_buffer(rova_sf, dist = rova_sf$fig_radius)
vama_buff <- st_buffer(vama_sf, dist = vama_sf$fig_radius)

# regsize_leg #ggplot with the pie legend
regsize_gleg <- gtable_filter(ggplot_gtable(ggplot_build(regsize_leg)), "guide-box")

# veg_leg #ggplot with habitat type legend (from script 03)
veg_gleg <- gtable_filter(ggplot_gtable(ggplot_build(veg_leg)), "guide-box")

# park boundary legend
bound_gleg <- gtable_filter(ggplot_gtable(ggplot_build(bound_leg)), "guide-box")
#library(gridExtra)

# Duplicates of 04_forest_summary_ROVA_code for testing convenience
rova_reg <- rova +  
  tm_shape(rova_sf %>% arrange(-fig_radius))+
  tm_symbols(size = "fig_area",
             scale = 2.5,
             size.max = max(regsize_nudge$fig_area),
             size.lim = c(min(regsize_nudge$fig_area), max(regsize_nudge$fig_area)), 
             perceptual = TRUE,
             shape = "Plot_Name", 
             group = "Charts", # pie charts
             shapes = rova_regsize_pies,
             border.col = NA, 
             border.lwd = NA)+
  {if(nrow(rova_sf %>% filter(totreg_std2 == 0))>0) # add symbols if no regen
    tm_shape(rova_sf %>% filter(totreg_std2 == 0))+
      tm_dots(shape = 24, border.col = "white", border.lwd = 1.8, size = 0.4)+
      tm_shape(rova_sf %>% filter(totreg_std2 == 0))+
      tm_dots(shape = 24, col = "#ff7f00", size = 0.3)}+
  
  tm_shape(rova_text)+ # add plot labels
  tm_text("plot_num", size = 0.6, #xmod = 0.01, ymod = -0.01,
          col = "black", bg.alpha = 0.8,
          just = 'center', shadow = "TRUE",
          fontface = 'bold')+
  # tm_shape(rova_buff)+
  # tm_borders(col = 'red')
  NULL

vama_reg <- vama +  
  tm_shape(vama_sf %>% arrange(-fig_radius))+
  tm_symbols(size = "fig_area",
             scale = 2.5, #2.5,
             size.max = max(regsize_nudge$fig_area),
             size.lim = c(min(regsize_nudge$fig_area), max(regsize_nudge$fig_area)), #diam
             perceptual = TRUE,
             shape = "Plot_Name", 
             group = "Charts", # pie charts
             shapes = vama_regsize_pies,
             border.col = NA, 
             border.lwd = NA)+
  {if(nrow(vama_sf %>% filter(totreg_std2 == 0))>0)
    tm_shape(vama_sf %>% filter(totreg_std2 == 0))+ # add symbols if no regen
      tm_dots(shape = 24, border.col = "white", border.lwd = 1.8, size = 0.4)+
      tm_shape(vama_sf %>% filter(totreg_std2 == 0))+
      tm_dots(shape = 24, col = "#ff7f00", size = 0.3)}+
  tm_shape(vama_text)+
  tm_text("plot_num", size = 0.6, 
          col = "black", bg.alpha = 0.8,
          just = 'center', shadow = "TRUE",
          fontface = 'bold')+
  # tm_shape(vama_buff)+
  # tm_borders(col = 'red')
  NULL


library(gridtext)
map1_text <- c("Trends in tree regeneration stem densities by size class in forest plots from the most recent survey cycle (2017
& 2019). Each plot is sampled on a 4-year cycle in an alternating panel, with plots 1-20 sampled in 2019, and
plots 21-40 sampled in 2017. Pie size is proportional to plot-level regeneration, and location may be shifted to
prevent overlap. Densites range from 0 to 12,732 stems/ha for an individual size class and only include native
canopy-forming species (differs from 2017 summary, which included all native species). Date: 6/13/2019.")

cap_grob<-textbox_grob(map1_text, hjust = 0, vjust = 1,
                       #x = 0, y = 0.98, 
                       padding = unit(c(0.3,0.3,0.3,0.3), "lines"), #t,r,b,l
                       gp = gpar(fontsize = 10, lineheight = 1.1),
                       box_gp = gpar(col = 'black', fill = "white"),
)
page_width <- 10.5
page_height <- 7 # banner is 1 in
map_layout <- grid.layout(nrow = 3, ncol = 2, # 2 rows/col for margins
                          #just = c("left"),
                          respect = TRUE, 
                          heights = unit(c(1, vama_yrat * page_height, elho_yrat * page_height),             
                                         c("in", "in", "in")),
                          widths = unit(c(col1_width * page_width, col2_width * page_width), 
                                        c("in", "in")))

map_vp <- viewport(name = "map_vp", layout = map_layout, #just = c("right", "center"),
                   width = unit(10.5, "in"), height = unit(8.5, "in"),
                   xscale = c(0, 1), yscale = c(0, 1))

ROVA_vp()

# grid.show.layout(map_layout)
# grid.show.viewport(map_vp)
# ROVA_vp()

ROVA_vp <- function(){
  grid.newpage()
  pushViewport(map_vp)
  # Map units
  #grid.rect(gp = gpar(col = 'black', fill = "#d4d4d4"))
  grid.rect(gp = gpar(col = "black", fill = "black"), 
            vp = viewport(layout.pos.row = 1, layout.pos.col = NULL))
  print(vama_reg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  grid.rect(gp = gpar(col = 'black', fill = NA), 
            vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(rova_reg, vp = viewport(layout.pos.row = 3, layout.pos.col = NULL))#,
  grid.rect(gp = gpar(col = "black", fill = NA),
            vp = viewport(layout.pos.row = 3, layout.pos.col = NULL))
  popViewport(1)
  # Title and Captions
  pushViewport(map_vp)
  # Create viewport in top right for captions and legends
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
  grid.rect(gp = gpar(col = "black", fill = "white"))
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 2,  #row 1 is title;2 caption; 3 legend;4 legend2
                                             heights = unit(c(0.125, 0.375, 0.39, 0.15), 
                                                            c("null", "null", "null", "null")),
                                             widths = unit(c(0.4, 0.6), c("null", "null")))))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.rect(gp = gpar(col = 'black', fill = "#bed2ff"))
  grid.text("Map 1. Tree Regeneration by Cycle", gp = gpar(fontsize = 16))
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  grid.draw(cap_grob)
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
  grid.draw(regsize_gleg)
  popViewport(1)
  pushViewport(viewport(x = unit(0.141, "native"), y = unit(0.063, "native")))
  grid.draw(bound_gleg)
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid.draw(veg_gleg)
  popViewport()
}

jpeg("ROVA_from_R.jpg", height = 8.5, width = 10.5, units = "in", res = 600)
ROVA_vp()
dev.off()

