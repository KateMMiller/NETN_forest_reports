#--------------------
# Set up park and metric specific templates
#--------------------

#----- Create veg map legend -----
# veg_leg <- tm_shape(park_veg)+
#   tm_fill(col = "fills")+
#   tm_add_legend(title = "Habitat types", type = 'fill', 
#                 labels = park_veg$labels,
#                 col = park_veg$fills,
#                 border.col = "white",
#                 border.lwd = 3,
#                 size = 0.5)+
#   tm_layout(legend.only = TRUE, 
#             legend.text.size = 1,
#             legend.title.size = 1.2, 
#             fontfamily = "A")

#----- Create pie legend -----
regsize_cols <- prep_sym_cols(df = map_controls, grp_var = "regsize")
regsize_symb <- prep_sym_shapes(df = map_controls, grp_var = "regsize") 

regsize_gen_df <- data.frame(totreg_std2 = c(rep(0.2, 5),0),
                             size_class = factor(c("sd15_30", "sd30_100", "sd100_150",
                                                   "sd150p", "sap", "nonereg")),
                             labs = c("Seedlings 15 \U2013 30cm", "Seedlings 30 \U2013 100cm",
                                        "Seedlings 100 \U2013 150cm", "Seedlings > 150cm",
                                        "Saplings (1 \U2013 9.9cm DBH)", "None present"))

regsize_leg <- ggplot(regsize_gen_df,
                      aes(x = size_class, y = totreg_std2, 
                          fill = size_class, size = size_class,
                          shape = size_class))+
  geom_point()+
  scale_fill_manual(name = "Stem densities by size class",
                    values = regsize_cols, 
                    breaks = regsize_gen_df$size_class,
                    labels = regsize_gen_df$labs)+
  scale_shape_manual(name = "Stem densities by size class",
                     labels = regsize_gen_df$labs,
                     values = as.numeric(regsize_symb))+
  scale_size_manual(name = "Stem densities by size class",
                    labels = regsize_gen_df$labs,
                    values = c(8, 8, 8, 8, 8, 3))+
  
  theme(legend.text = element_text(size = 11),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank())





