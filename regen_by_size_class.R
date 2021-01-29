library(tidyverse)
library(sf)
library(forestNETN)

importData()

#----- Prep forest data -----
reg <- joinRegenData(park = park_code, speciesType = "native", canopyForm = 'all', 
                     from = 2016, to = 2019)
reg1 <- left_join(reg, units[,c("Plot_Name", "Unit")], by = "Plot_Name")

reg2 <- reg1 %>% group_by(Unit_Code, Plot_Name, Unit, Plot_Number, X_Coord, Y_Coord) %>% 
  summarise(sd15_30 = sum(seed15.30, na.rm = TRUE), 
            sd30_100 = sum(seed30.100, na.rm = TRUE), 
            sd100_150 = sum(seed100.150, na.rm = TRUE), 
            sd150p = sum(seed150p, na.rm = TRUE),
            sap = sum(sap.den, na.rm = TRUE),
            totreg_m2 = (sd15_30 + sd30_100 + sd100_150 + sd150p +sap)/10000, 
            .groups = 'keep') %>% ungroup()

min_totreg <- min(reg2$totreg_m2)
diff_totreg <- diff(range(reg2$totreg_m2))

reg2 <- reg2 %>% mutate(totreg_std = (reg2$totreg_m2 - min_totreg) / (diff_totreg),
                        totreg_std2 = ifelse(totreg_std < 0.1, 0.1, totreg_std),
                        pie_exp = pie_min + (pie_max - pie_min)*(totreg_std2)
)

# Create long list for ggplot pie chart list
plot_list <- sort(unique(reg2$Plot_Name))

reg_long <- reg2 %>% select(Plot_Name, sd15_30:sap) %>% 
  pivot_longer(cols = c(-Plot_Name), 
               names_to = "size_class", 
               values_to = "dens") %>% 
  group_by(Plot_Name) %>% 
  mutate(totdens = sum(dens))

reg_long$size_class <- factor(reg_long$size_class,
                              levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))

reg_long <- reg_long %>% arrange(Plot_Name, size_class) 
reg_long2 <- left_join(reg_long, reg2, by = "Plot_Name") 

#----- Check and fix pie overlap -----
# Convert forest data to simple feature

CRS <- if(park_code %in% c('ACAD', 'MIMA')){26919} else {26918}
reg2 <- reg2 %>% mutate(plot_num = as.numeric(Plot_Number), fig_radius = pie_exp * pie_expfac)
reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = CRS, agr = "constant")
reg_sf_alb <- st_transform(reg_sf, crs = 5070)

# Convert forest data to simple feature
CRS <- if(park_code %in% c('ACAD', 'MIMA')){26919} else {26918}
reg2 <- reg2 %>% mutate(plot_num = as.numeric(Plot_Number), fig_radius = pie_exp * pie_expfac)

reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = CRS, agr = "constant")
reg_sf_alb <- st_transform(reg_sf, crs = 5070)
#st_crs(reg_sf_alb) # Conus Albers Equal Area

reg_df <- cbind(st_drop_geometry(reg_sf_alb), st_coordinates(reg_sf_alb))
reg_sf <- st_as_sf(reg_df, coords = c("X", "Y"), crs = 5070)
reg_sf <- st_buffer(reg_sf, reg_sf$fig_radius)
check_overlap(reg_sf)

# nums <- 20
# nudge_df <- nudge_XY(reg_df, x = "X", y = "Y", stdvar = "totreg_std2", nums) #
# nudge_sf <- st_as_sf(nudge_df, coords = c("X_nudge", "Y_nudge"), crs = 5070) # 


#----- Create pie legend -----

pie_for_leg <- ggplot(reg_long2 %>% filter(Plot_Name == "ROVA-001"),
                      aes(x = "", y = totreg_std2, fill = size_class))+
  geom_bar(stat = 'identity', width = 1, color = '#696969')+
  scale_fill_manual(values = regsize_cols, name = "Stem densities by size class",
                    labels = c("Seedlings 15 \U2013 30cm", "Seedlings 30 \U2013 100cm",
                               "Seedlings 100 \U2013 150cm", "Seedlings > 150cm",
                               "Saplings (1 \U2013 9.9cm DBH"))+ 
  coord_polar(theta = "y", start= 0)+
  theme(legend.key = element_rect(size = 6, fill = "white", color = NA),
        legend.key.height = unit(15, 'pt'),
        legend.key.width = unit(15,'pt'))


