#-------------------
# Testing code to make park-level template
#-------------------
library(grid)

#----- Set up park controls -----
# park_code = "ROVA"
# pie_min = 0.3 #0.5 will be 0.7, actually, which is better
# pie_max = 2#1.5 in parks where pies are packed (eg ACAD)

# Load source file that loads libraries, imports data and shapefiles and prepares for plottingh
source("./plot_setup_code.R") # sets up shapefiles and pie function
source("./nudge_XY.R")
source("./nudge_XY_sing.R") # drop after added to package
source("./check_overlap.R") # drop after added to package

# Mapping ROVA requires plotting over multiple tmap panels. To ensure the panels are plotted at the same
# map scale (1:24000 for ROVA), we need to load bounding boxes drawn from their original ArcMap projects
rova_bb <- st_bbox(park_veg)
vama_bb <- st_buffer(park_bound %>% filter(Unit == "VAMA"), dist = 250)
elho_bb <- st_buffer(park_bound %>% filter(Unit != "VAMA"), dist = 250)

vama_yrange <- st_bbox(vama_bb)[4] - st_bbox(vama_bb)[2]
elho_yrange <- st_bbox(elho_bb)[4] - st_bbox(elho_bb)[2] 
vama_xrange <- st_bbox(vama_bb)[3] - st_bbox(vama_bb)[1]
elho_xrange <- st_bbox(elho_bb)[3] - st_bbox(elho_bb)[1] 

scales_tbl <- data.frame(unit = c("VAMA", "ROVA"), 
                         xrange = c(vama_xrange/(vama_xrange + elho_xrange),
                                    elho_xrange/(vama_xrange + elho_xrange)),
                         yrange = c(vama_yrange/(vama_yrange + elho_yrange),
                                    elho_yrange/(vama_yrange + elho_yrange))
                         )

rova_bb
scales_tbl

# Code to control scale across plots
vama <-   tm_shape(vama_bb, projection = 26918, is.master = T) + 
  tm_fill(alpha = 0)+
  tm_shape(park_veg, projection = 26918) +
  tm_fill("fills") +
  tm_shape(park_bound) +
  tm_borders(col = 'black', lwd = 2)+ 
  tm_shape(park_bound %>% filter(Unit == "VAMA"), 
           projection = 26918) +
  tm_borders(col = "black", lwd = 2)#+
  # tm_scale_bar(breaks = c(0,0.25,0.5))
  #tm_layout(frame = FALSE)

vama

rova <- tm_shape(elho_bb, projection = 26918, is.master = T) + 
  tm_fill(alpha = 0)+
  tm_shape(park_veg, projection = 26918) +
  tm_fill("fills") +
  tm_shape(park_bound) +
  tm_borders(col = 'black', lwd = 2)+ 
  tm_shape(park_bound %>% filter(Unit != "VAMA"),  is.master = TRUE, 
           projection = 26918) +
  tm_borders(col = "black", lwd = 2) #+
  # tm_compass(size = 2, position = c(0.95,0.01), just = 0.5) + 
  # tm_scale_bar(breaks = c(0,0.25,0.5,0.75,1), just = 0.5, 
  #              position = c(0.5,-0.01))#+
  # tm_layout(
  #   frame = FALSE#,
  #   #outer.margins = c(0.02,0.02,1,0.02)
  #   #inner.margins = c(0.2,0.1,0.1,0.1)
  # )#
rova

# jpeg("ROVA_template.jpg", width = 10.3849, height = 8.1388, units = "in", res = 600)
# grid.newpage()
# pushViewport(
#   viewport(
#     layout = grid.layout(nrow = 2, ncol = 2, heights = unit(c(scales_tbl$yrange[scales_tbl$unit == "VAMA"], 
#                                                               scales_tbl$yrange[scales_tbl$unit == "ROVA"]), 
#                                                              "null"),
#                          widths = unit(c(scales_tbl$xrange[scales_tbl$unit == "VAMA"], 
#                                          scales_tbl$xrange[scales_tbl$unit == "ROVA"]), 
#                                        "null"),
#                          just = 'center')
#   )
# )
# 
# print(vama, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(rova, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
# dev.off()

