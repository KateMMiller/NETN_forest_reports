
# row1_height * page_height+
# row2_height * page_height
# 
# col1_width * page_width+
# col2_width * page_width
# 
# (row1_height * page_height)/(col1_width * page_width)
# (row2_height * page_height)/((col2_width + col1_width) * page_width)

vama_grob <- tmap_grob(vama_reg)
rova_grob <- tmap_grob(rova_reg)

#vama_grob$name <- "vama_grob"
# vama_grob$vp[[1]]$height <- unit(row1_height * page_height, "in")
# vama_grob$vp[[1]]$width <- unit(col1_width * page_width, "in")
# vama_grob$vp[[2]]$height <- unit(row1_height * page_height, "in")
# vama_grob$vp[[2]]$width <- unit(col1_width * page_width, "in")
# str(vama_grob$vp[[2]])
# 
# #rova_grob$name <- "rova_grob"
# rova_grob$vp[[1]]$height <- unit(row2_height * page_height, "in")
# rova_grob$vp[[1]]$width <- unit((col2_width+col1_width)*page_width, "in")
# rova_grob$vp[[2]]$height <- unit(row2_height * page_height, "in")
# rova_grob$vp[[2]]$width <- unit((col2_width+col1_width)*page_width, "in")
