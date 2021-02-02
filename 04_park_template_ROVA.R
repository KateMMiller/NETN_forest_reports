#-------------------
# Package tmap template for ROVA
#-------------------

#+++++++++++++++++++++++++
# NOTE: This script assumes 01_compile_base_layers.R, 02_calculate_metric_functions.R, and
# 03_compile_map_template.R have already been sourced. This is not a stand alone script
#+++++++++++++++++++++++++

#-------------------------
# Mapping ROVA requires plotting over multiple tmap panels. To ensure the panels are plotted at the same
# map scale (1:24000 for ROVA), we need to set each unit's range proportional to the full bounding box of the park
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


# VAMA basemap
vama <-   tm_shape(vama_bb, projection = 26918, is.master = T) + 
  tm_fill(alpha = 0)+
  tm_shape(park_veg, projection = 26918) +
  tm_fill("fills") +
  tm_shape(park_bound) +
  tm_borders(col = 'black', lwd = 2)+ 
  tm_shape(park_bound %>% filter(Unit == "VAMA"), 
           projection = 26918) +
  tm_borders(col = "black", lwd = 2)#+
# tm_scale_bar(breaks = c(0,0.25,0.5))  # still here to check scale occasionally

# ROVA basemap sans VAMA
rova <- tm_shape(elho_bb, projection = 26918, is.master = T) + 
  tm_fill(alpha = 0)+
  tm_shape(park_veg, projection = 26918) +
  tm_fill("fills") +
  tm_shape(park_bound) +
  tm_borders(col = 'black', lwd = 2)+ 
  tm_shape(park_bound %>% filter(Unit != "VAMA"),  is.master = TRUE, 
           projection = 26918) +
  tm_borders(col = "black", lwd = 2) 



