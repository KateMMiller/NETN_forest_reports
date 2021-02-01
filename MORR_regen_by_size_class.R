#----- Set up Park controls -----
# devtools::install_github("mtennekes/tmap") # installing beta version for tmap_grob
# devtools::install_github("mtennekes/tmaptools")

library(tmap)
library(tmaptools)
library(gtable)

windowsFonts(A = windowsFont("Arial"))

park_code = "MORR"
pie_min = 0.3 #0.5 will be 0.7, actually, which is better
pie_max = 2 #1.5 in parks where pies are packed (eg ROVA, ACAD)

#source("./ROVA_template.R")
source("./plot_setup_code.R") # sets up shapefiles and pie function
source("./nudge_XY.R")
source("./nudge_XY_sing.R") # drop after added to package
source("./check_overlap.R") # drop after added to package
source("./regen_by_size_class.R")

nums <- 30
nudge_df <- nudge_XY(reg_df, x = "X", y = "Y", stdvar = "totreg_std2", nums) #
nudge_sf <- st_as_sf(nudge_df, coords = c("X_nudge", "Y_nudge"), crs = 5070) # 
write_sf(morr_sf, "./shapefiles/MORR_regensize_pie_nudge.shp")

morr_list <- sort(unique(reg_long2$Plot_Name))

morr_pies <- map(morr_list, ~pie_regsize_fun(reg_long2, .x, y_var = dens, 
                                             grp_var = size_class, std_var = "totreg_std2")) %>% 
  set_names(morr_list)

morr_sf <- st_as_sf(nudge_df, coords = c("X_nudge", "Y_nudge"), crs = 5070) 

morr_base <- tm_shape(park_veg, projection = 26918) +
             tm_fill("fills") +
             tm_shape(park_bound) +
             tm_borders(col = 'black', lwd = 2)+ 
             tm_shape(park_bound, projection = 26918) +
             tm_borders(col = "black", lwd = 2)#+

morr_reg <- morr_base +  
  tm_shape(morr_sf)+
  tm_symbols(shape = "Plot_Name", group = "Charts",
             shapes = morr_pies, border.col = NA, border.lwd = NA)+
  tm_layout(inner.margins = 0, #outer.margins = 0.01,
            outer.margins = c(0.02, 0.00832, 0.008, 0.01),
            bg.color = "#d4d4d4",
            fontfamily = "A")+
  tm_compass(size = 2, position = c(0.95,0.03), just = 0.5) + 
  tm_scale_bar(breaks = c(0,0.25,0.5,0.75,1),  just = 0.5, 
               position = c(0.5,0.03))+
  #tm_text("Plot_Number", size = 0.8)+
  tm_legend(show = FALSE)

morr_reg

pie_leg <- gtable_filter(ggplot_gtable(ggplot_build(pie_for_leg)), "guide-box")
pie_leg

map_vp <-
  viewport(layout = grid.layout(nrow = 2, ncol = 2,
                                heights = unit(c(scales_tbl$yrange[scales_tbl$unit == "VAMA"],
                                                 scales_tbl$yrange[scales_tbl$unit == "ROVA"]),
                                               c("null", "null")),
                                widths = unit(c(scales_tbl$xrange[scales_tbl$unit == "VAMA"],
                                                scales_tbl$xrange[scales_tbl$unit == "ROVA"]),
                                              c("null", "null"))
  ))

#jpeg(paste0(name,'.jpg'), width = 9.9, height = 8.5, units = "in", res = 600)
library(grid)
grid.newpage()
print(morr_reg, viewport(just = "center"))
print(veg_leg, viewport(x = 0.2, y = 0, height = unit(0.2, "npc"))) 
pushViewport(viewport(#layout.pos.row = 1, layout.pos.col = 2,
  x = 0.7, y = 0.85))
grid.draw(pie_leg)

rova_grid("ROVA_template_Arial", rova_reg, vama_reg)

rova_grid <- function(name, rova_obj, vama_obj){
  map_vp <-
    viewport(layout = grid.layout(nrow = 2, ncol = 2,
                                  heights = unit(c(scales_tbl$yrange[scales_tbl$unit == "VAMA"],
                                                   scales_tbl$yrange[scales_tbl$unit == "ROVA"]),
                                                 c("null", "null")),
                                  widths = unit(c(scales_tbl$xrange[scales_tbl$unit == "VAMA"],
                                                  scales_tbl$xrange[scales_tbl$unit == "ROVA"]),
                                                c("null", "null"))
    ))
  jpeg(paste0(name,'.jpg'), width = 9.9, height = 8.5, units = "in", res = 600)
  grid.newpage()
  pushViewport(map_vp)
  grid.rect(gp = gpar(col = 'black', fill = "white"))
  print(rova_obj, viewport(layout.pos.row = 2, layout.pos.col = NULL, just = "center"))
  print(vama_obj, viewport(layout.pos.row = 1, 
                           layout.pos.col = 1, 
                           just = "center"))
  print(veg_leg, viewport(layout.pos.row = 1, layout.pos.col = 2,
                          x = -0.1, y = 0, height = unit(0.2, "npc"))) 
  pushViewport(viewport(#layout.pos.row = 1, layout.pos.col = 2,
    x = 0.7, y = 0.85))
  grid.draw(pie_leg)
  dev.off()
}


