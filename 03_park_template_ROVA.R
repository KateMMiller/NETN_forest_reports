#-------------------
# Package tmap template for ROVA
#-------------------
library(tmap)
library(sf)
library(tmaptools)

#+++++++++++++++++++++++++
# NOTE: This script assumes 01_compile_base_layers.R, 02_calculate_metric_functions.R, and
# 03_compile_map_template.R have already been sourced. This is not a stand alone script
#+++++++++++++++++++++++++

#-------------------------
# Mapping ROVA requires plotting over multiple tmap panels. To ensure the panels are plotted at the same
# map scale (1:24000 for ROVA), we need to set each unit's range proportional to the full bounding box of the park
# rova_bb <- st_bbox(park_veg)
#rova_buff_bound <- st_buffer(park_bound, dist = 250)
rova_buff_bound <- st_buffer(park_bound, dist = 200)
#rova_vama_buff <- st_buffer(park_bound, dist = 200)

overlap <- st_bbox(rova_buff_bound %>% filter(Unit != "VAMA"))[1] -
             st_bbox(rova_buff_bound %>% filter(Unit == "VAMA"))[1]  #697.506

st_bbox(rova_buff_bound)
rova_bbox <- st_bbox(rova_buff_bound %>% filter(Unit != "VAMA"))
vama_bbox <- st_bbox(rova_buff_bound %>% filter(Unit == "VAMA"))

# calculating column widths
#rova_xrange <- st_bbox(rova_buff_bound)[3] - st_bbox(rova_vama_buff)[1] - overlap[1] # full width of ROVA - VAMA overlap
rova_xrange <- st_bbox(rova_buff_bound)[3] - st_bbox(rova_buff_bound)[1] - overlap[1] # full width of ROVA - VAMA overlap

rova_xrange
vama_xrange <- st_bbox(rova_buff_bound %>% filter(Unit == 'VAMA'))[3] - # width of VAMA
                 st_bbox(rova_buff_bound %>% filter(Unit == 'VAMA'))[1] 

# elho_xrange <- st_bbox(rova_vama_buff %>% filter(Unit != 'VAMA'))[3] - # width of ELRO/HOFR
#   st_bbox(rova_vama_buff %>% filter(Unit != 'VAMA'))[1] - overlap[1] 

elho_xrange <- st_bbox(rova_buff_bound %>% filter(Unit != 'VAMA'))[3] - # width of ELRO/HOFR
               st_bbox(rova_buff_bound %>% filter(Unit != 'VAMA'))[1] #- overlap[1] 

vama_xrat <- vama_xrange/rova_xrange  # Ratio of VAMA to whole ROVA

elho_part_xrat <- (elho_xrange/rova_xrange) - vama_xrat # Ratio of ELRO/HOFR to whole ROVA - ratio of VAMA (b/c 2 cols)

col1_width <- (vama_xrat)/(elho_part_xrat + vama_xrat) # rescale so both units add to 1
col2_width <- (elho_part_xrat)/(elho_part_xrat + vama_xrat) 
col1_width + col2_width #1

# calculating row heights

vama_yrange <- st_bbox(rova_buff_bound %>% filter(Unit == 'VAMA'))[4] - # full height of VAMA
                st_bbox(rova_buff_bound %>% filter(Unit == 'VAMA'))[2] 
elho_yrange <- st_bbox(rova_buff_bound %>% filter(Unit != 'VAMA'))[4] - # full height of ELRO/HOFR
                st_bbox(rova_buff_bound %>% filter(Unit != 'VAMA'))[2] 

rova_yrange <- vama_yrange + elho_yrange #don't care about the vertical space between VAMA and ELHO

vama_yrat <- vama_yrange/rova_yrange # Ratio of VAMA to whole ROVA
elho_yrat <- elho_yrange/rova_yrange # Ratio of ELRO/HOFR to whole ROVA

vama_yrat + elho_yrat
# each unit has its own row, so don't need to subtract vama, like col widths

#rova_250_18 <- st_transform(rova_buff_bound, crs = 26918)
#rova_200_18 <- st_transform(rova_vama_buff, crs = 26918)
# bb250 <- st_bbox(rova_buff_bound %>% filter(Unit == "VAMA"))
# # bb200 <- st_bbox(rova_vama_buff %>% filter(Unit == "VAMA"))
# 
# vama_bbox <- c(bb250$xmin - 50, bb200$ymin, bb250$xmax + 50, bb200$ymax)
# vama_bbox <- c(bb250$xmin, bb200$ymin, bb250$xmax, bb200$ymax)
# vama_bbox

vama <-   
  tm_shape(park_veg, projection = 26918, bbox = st_bbox(vama_bbox))+#, bbox = vama_bbox) +
  tm_fill("fills", alpha = 0.8) +
  #tm_shape(rova_buff_bnd, is.master = TRUE) +
  #tm_borders(col = 'black', lwd = 2)+
  tm_shape(park_bound %>% filter(Unit == "VAMA"), 
           projection = 26918) +
  tm_borders(col = "#6a6a6a", lwd = 2) +
  tm_legend(show = FALSE) +
  tm_layout(inner.margins = c(0.025, 0.03, 0, 0), 
            #outer.margins = c(0.01, 0.01, 0.01, 0.01),
            #outer.margins = c(0.02, 0.00832, 0.008, 0.01),
            #outer.margins = c(0, 0.04, 0.01, 0.01),
            bg.color = "#d4d4d4",
            frame = FALSE,
            fontfamily = "A")+
  #tm_scale_bar(breaks = c(0,0.25,0.5))+  # still here to check scale occasionally
  NULL

# ROVA basemap sans VAMA
rova <- tm_shape(rova_buff_bound %>% filter(Unit != "VAMA"), projection = 26918, 
                 is.master = TRUE) + 
  tm_fill(alpha = 0 )+
  tm_shape(park_veg, projection = 26918) +
  tm_fill("fills", alpha = 0.8) +
  #tm_shape(rova_buff_bnd, is.master = TRUE) +
  #tm_borders(col = 'black', lwd = 2)+
  # tm_shape(park_bound) +
  # tm_borders(col = 'black', lwd = 2)+
  tm_shape(park_bound %>% filter(Unit != "VAMA"),  
           projection = 26918) +
  tm_borders(col = "#6a6a6a", lwd = 2) +
  tm_compass(size = 2, position = c(0.95,0.03), just = 0.5) + 
  tm_scale_bar(breaks = c(0,0.25,0.5,0.75,1),  just = 0.5, 
               position = c(0.5,0.03)) +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), 
            #outer.margins = 0,
            #outer.margins = c(0.02, 0.00832, 0.008, 0.01),
            frame = FALSE,
            bg.color = "#d4d4d4",
            fontfamily = "A")+
  tm_legend(show = FALSE)
