#-------------------------------
# Pie size controls 
#-------------------------------
pie_expfac1 <- data.frame(park_code = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"),
                          pie_expfac = c(700, 50, 80, 80, 80, 25, 90, 22)) #~trial and error based on park's ideal map scale  

pie_expfac <- pie_expfac1$pie_expfac[pie_expfac1$park_code == park_code] 

#--------------------
# Park and metric specific templates
#--------------------

#----- regsize pie legend -----
# fake dataset for leg
regsize_cols <- prep_sym_cols(df = map_controls, grp_var = "regsize")
regsize_symb <- prep_sym_shapes(df = map_controls, grp_var = "regsize") 

regsize_gen_df <- data.frame(totreg_std2 = c(rep(0.2, 5),0),
                             size_class = factor(c("sd15_30", "sd30_100", "sd100_150",
                                                   "sd150p", "sap", "nonereg")),
                             labs = c("Seedlings 15 \U2013 30cm", "Seedlings 30 \U2013 100cm",
                                        "Seedlings 100 \U2013 150cm", "Seedlings > 150cm",
                                        "Saplings (1 \U2013 9.9cm DBH)", "None present"))
# fake plot for leg
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
  theme_void()+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank())





