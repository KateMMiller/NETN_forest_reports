



ROVA_vp()

#grid.locator()
# Save map
#jpeg("ROVA_plot_text_nudge_1p2.jpg", height = 8.5, width = 10.5, units = "in", res = 600)

ROVA_vp <- function(){grid.newpage()
  pushViewport(map_vp)
  # Map units
  grid.rect(gp = gpar(col = 'black', fill = "grey"))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
  #grid.rect(gp = gpar(col = "black", fill = NA))
  print(vama_reg, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) #add x = 0
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2:4))
  #grid.rect(gp = gpar(col = "black", fill = NA))
  print(rova_reg, vp = viewport(layout.pos.row = 4, layout.pos.col = 2:4))
  popViewport(1)
  # Title and Captions
  pushViewport(map_vp)
  # Create viewport in top right for captions and legends
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 3:4))
  grid.rect(gp = gpar(col = "black", fill = "white"))
  
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 2,  #row 1 is title;2 caption; 3 legend;4 legend2
                                             heights = unit(c(0.15, 0.32, 0.43, 0.1), 
                                                            c("null", "null", "null", "null")),
                                             widths = unit(c(0.4, 0.6), c("null", "null")))))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.rect(gp = gpar(col = 'black', fill = "#bed2ff"))
  grid.text("Map 1. Tree Regeneration by Cycle", gp = gpar(fontsize = 16))
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))#, just = c("left", "top")))
  grid.draw(cap_grob)
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))#, x = 0.5, just = c("left", "top")))
  grid.draw(regsize_gleg)
  popViewport(1)
  pushViewport(viewport(x = unit(0.132, "native"), y = unit(0.061, "native")))
  grid.draw(bound_gleg)
  popViewport(1)
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid.draw(veg_gleg)
  }


