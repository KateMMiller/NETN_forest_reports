

#----- Create veg map legend -----
windowsFonts(A = windowsFont("Arial"))

veg_leg <- tm_shape(park_veg)+
  tm_fill(col = "fills")+
  tm_add_legend(title = "Habitat types", type = 'fill', 
                labels = park_veg$labels,
                col = park_veg$fills,
                border.col = "white",
                border.lwd = 3,
                size = 0.5)+
  tm_layout(legend.only = TRUE, 
            legend.text.size = 1,
            legend.title.size = 1.2, 
            fontfamily = "A")