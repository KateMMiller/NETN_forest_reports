library(tidyverse)
library(sf) 
library(forestNETN)

#-------------------------------
# Regen by size class 
#-------------------------------
# Prep data
regen_by_sizeclass <- function(park_code, year_start, year_end){
  reg <- joinRegenData(park = park_code, speciesType = "native", canopyForm = 'canopy', 
                       from = year_start, to = year_end)
  
  units <- read.csv("tbl_Alternative_Plot_Labels.csv")

  reg1 <- left_join(reg, units[,c("Plot_Name", "Unit")], by = "Plot_Name")

  reg2 <- reg1 %>% group_by(Unit_Code, Plot_Name, Unit, Plot_Number, X_Coord, Y_Coord) %>% 
                   summarise(sd15_30 = sum(seed15.30, na.rm = TRUE), 
                             sd30_100 = sum(seed30.100, na.rm = TRUE), 
                             sd100_150 = sum(seed100.150, na.rm = TRUE), 
                             sd150p = sum(seed150p, na.rm = TRUE),
                             sap = sum(sap.den, na.rm = TRUE),
                             totreg_m2 = (sd15_30 + sd30_100 + sd100_150 + sd150p +sap)/10000, 
                             .groups = 'keep') %>% 
                   ungroup()

min_totreg <- min(reg2$totreg_m2)
diff_totreg <- diff(range(reg2$totreg_m2))

reg2 <- reg2 %>% mutate(totreg_std = (reg2$totreg_m2 - min_totreg) / (diff_totreg),
                        totreg_std2 = ifelse(totreg_std > 0 & totreg_std < 0.1, 0.1, totreg_std),
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

# Convert output data to simple feature and transform to UTM Albers (5070)
CRS <- if(park_code %in% c('ACAD', 'MIMA')){26919} else {26918} #coords from database
reg2 <- reg2 %>% mutate(plot_num = as.numeric(Plot_Number), #use for plot labels later
                        fig_radius = pie_exp * pie_expfac) #approximates size of pie
reg_sf <- st_as_sf(reg2, coords = c("X_Coord", "Y_Coord"), crs = CRS, agr = "constant") 
reg_sf_alb <- st_transform(reg_sf, crs = 5070) #convert to UTM Albers

reg_df <- cbind(st_drop_geometry(reg_sf_alb), st_coordinates(reg_sf_alb)) #strip new coords in Albers
#reg_sf <- st_as_sf(reg_df, coords = c("X", "Y"), crs = 5070) #make into sf with new CRS

assign("regsize_long", reg_long2, envir = .GlobalEnv) # assign output to global env.
assign("regsize_df", reg_df, envir = .GlobalEnv)
#assign("reg_size_sf", reg_sf, envir = .GlobalEnv)
}

