#----- Set up Park controls -----
# devtools::install_github("mtennekes/tmap") # installing beta version for tmap_grob
# devtools::install_github("mtennekes/tmaptools")

library(tmap)
library(tmaptools)
library(gtable)
park_code = "ROVA"
pie_min = 0.3 #0.5 will be 0.7, actually, which is better
pie_max = 1.5#1.5 in parks where pies are packed (eg ACAD)

source("./ROVA_template.R")
source("./regen_by_size_class.R")

nums <- 30
nudge_df <- nudge_XY(reg_df, x = "X", y = "Y", stdvar = "totreg_std2", nums) #
nudge_sf <- st_as_sf(nudge_df, coords = c("X_nudge", "Y_nudge"), crs = 5070) # 

rova_list <- sort(unique(reg2$Plot_Name[reg2$Unit != "VAMA"]))
vama_list <- sort(unique(reg2$Plot_Name[reg2$Unit == "VAMA"]))

vama_pies <- map(vama_list, ~pie_fun(reg_long2 %>% filter(Unit == "VAMA"), .x, y_var = dens, 
                                     grp_var = size_class, std_var = "totreg_std2")) %>% 
                 set_names(vama_list)

rova_pies <- map(rova_list, ~pie_fun(reg_long2 %>% filter(Unit !="VAMA"), .x, y_var = dens, 
                                     grp_var = size_class, std_var = "totreg_std2")) %>% 
             set_names(rova_list)

rova_sf <- st_as_sf(nudge_df %>% filter(Unit != "VAMA"), coords = c("X_nudge", "Y_nudge"), crs = 5070) 
vama_sf <- st_as_sf(nudge_df %>% filter(Unit == "VAMA"), coords = c("X_nudge", "Y_nudge"), crs = 5070)

rova_reg <- rova +  
  tm_shape(rova_sf)+
  tm_symbols(shape = "Plot_Name", group = "Charts",
             shapes = rova_pies, border.col = NA, border.lwd = NA)+
  tm_layout(inner.margins = 0, #outer.margins = 0.01,
            outer.margins = c(0.02, 0.00832, 0.008, 0.01),
            bg.color = "#d4d4d4")+
  tm_compass(size = 2, position = c(0.95,0.03), just = 0.5) + 
  tm_scale_bar(breaks = c(0,0.25,0.5,0.75,1),  just = 0.5, 
               position = c(0.5,0.03))+
  #tm_text("Plot_Number", size = 0.8)+
  tm_legend(show = FALSE)


vama_reg <- vama +  
  tm_shape(vama_sf)+
  tm_symbols(shape = "Plot_Name", group = "Charts",
             shapes = vama_pies, border.col = NA, border.lwd = NA)+
  tm_layout(inner.margins = 0, #outer.margins = 0.01,
            outer.margins = c(0, 0.04, 0.01, 0.01),
            bg.color = "#d4d4d4")+
  # tm_scale_bar(breaks = c(0,0.25,0.5,0.75,1),  just = 0.5, 
  #              position = c(0.5,0.03))+
  #tm_text("Plot_Number", size = 0.8)+
  tm_legend(show = FALSE)

#veg_leg is the vegmap legend coming from plot_setup_code
#pie_leg is the piechart legend coming from regen_by_size_class
pie_leg <- gtable_filter(ggplot_gtable(ggplot_build(pie_for_leg)), "guide-box")

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
grid.newpage()
pushViewport(map_vp)
grid.rect(gp = gpar(col = 'black', fill = "white"))
print(rova_reg, viewport(layout.pos.row = 2, layout.pos.col = NULL, just = "center"))
print(vama_reg, viewport(layout.pos.row = 1, 
                         layout.pos.col = 1, 
                         just = "center"))
print(veg_leg, viewport(layout.pos.row = 1, layout.pos.col = 2,
                        x = -0.1, y = 0, height = unit(0.2, "npc"))) 
pushViewport(viewport(#layout.pos.row = 1, layout.pos.col = 2,
                      x = 0.7, y = 0.85))
grid.draw(pie_leg)

?viewport

rova_grid("ROVA_template", rova_reg, vama_reg)

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

