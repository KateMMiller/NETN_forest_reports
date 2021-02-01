# vama_grb <- tmap_grob(vama_reg)
# rova_grb <- tmap_grob(rova_reg)
# 
# map_vp <- 
#     viewport(#x = 0, y = 0, 
#              width = unit(10.3849, "in"), height = unit(8.13, "in"),
# #             xscale = c(0,10.3849), yscale = c(0,8.13),
#              layout = grid.layout(nrow = 2, ncol = 2, 
#                                   heights = unit(c(scales_tbl$yrange[scales_tbl$unit == "VAMA"],
#                                                    scales_tbl$yrange[scales_tbl$unit == "ROVA"]), 
#                                                  c("null", "null")),
#                                   widths = unit(c(scales_tbl$xrange[scales_tbl$unit == "VAMA"], 
#                                                   scales_tbl$xrange[scales_tbl$unit == "ROVA"]), 
#                                                 c("null", "null"))
#                                   ),
#               name = "map_vp")

# vama_vp <- viewport(x = 0, layout.pos.row = 1, layout.pos.col = 1, just = 'left', name = "vama_vp")
# rova_vp <- viewport(x = 0, layout.pos.row = 2, layout.pos.col = NULL, just = 'left', name = "rova_vp")


# tmap_options()$outer.margins
# tmap_options()$inner.margins
# tmap_options()$between.margin

#resave("regen4")

#resave <- function(name){
#jpeg(paste0(name,'.jpg'), width = 10.3849, height = 8.13, units = "in", res = 600)
# grid.newpage()

#map_vp <- 
#pushViewport(
#    viewport(#x = 0, y = 0,
#             layout = grid.layout(nrow = 2, ncol = 2, 
#                                  heights = unit(c(scales_tbl$yrange[scales_tbl$unit == "VAMA"],
#                                                   scales_tbl$yrange[scales_tbl$unit == "ROVA"]), 
#                                                 c("null", "null")),
#                                  widths = unit(c(scales_tbl$xrange[scales_tbl$unit == "VAMA"], 
#                                                  scales_tbl$xrange[scales_tbl$unit == "ROVA"]), 
#                                                c("null", "null"))
#                                  ))#)
# 
# vama_vp <- viewport(layout.pos.row = 1, layout.pos.col = 1, name = "vama_vp")
# rova_vp <- viewport(layout.pos.row = 2, layout.pos.col = NULL, name = "rova_vp")

# #maptree <- vpTree(map_vp, vpList(vama_vp, rova_vp))
# pushViewport(maptree)
# seekViewport("vama_vp")
# grid.draw(vama_grb)
# upViewport(0)
# seekViewport("rova_vp")
# grid.draw(rova_grb)
# 
# grid.locator()
# print(vama_reg, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(rova_reg, vp = viewport(layout.pos.row = 3, layout.pos.col = NULL))

grid.locator()


