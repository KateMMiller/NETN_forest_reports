library(forestNETN)
library(tidyverse)
library(rgdal)
library(sf)
library(scatterpie)
library(ggpolypath)
library(shadowtext)
library(ggspatial)
library(rgeos)
library(cowplot)
#library(plotly)

park_code = "SARA"
park_long_name = "Saratoga National Historical Park"

importData()

bounds <- readOGR("./shapefiles/NETN_park_bounds.shp", layer = "NETN_park_bounds", 
                  GDAL1_integer64_policy = TRUE)

vegmap <- readOGR("./shapefiles/NETN_vegmap_simplified.shp", 
                  layer = "NETN_vegmap_simplified", GDAL1_integer64_policy = TRUE)

plots <- readOGR("./shapefiles/NETN_forest_plots_dd.shp", layer = "NETN_forest_plots_dd",
                 GDAL1_integer64_policy = TRUE)


park_bound <- bounds[bounds$Park == park_code, ]
park_veg <- vegmap[vegmap$Park == park_code, ]
park_plots <- plots[plots$Unit_Cd == park_code, ]

map_controls <- read.csv("./shapefiles/map_controls.csv")
veg_colors <- read.csv("./shapefiles/Vegmap_colors.csv")

veg_controls = data.frame(values = c(veg_colors$veg_type, park_code),
                          x = NA,
                          y = NA,
                          breaks = c(veg_colors$veg_type, park_code),
                          group = rep("vegmap", nrow(veg_colors)+1),
                          labels = c(veg_colors$labels, "Park boundary"),
                          fills = c(veg_colors$fill, NA),
                          shapes = NA,
                          sizes = NA
)

none_controls <- data.frame(values = "nonereg",
                            x = 6,
                            y = 6, 
                            breaks = "nonereg",
                            group = "regsize",
                            labels = "None present",
                            fills = "#ff7f00",
                            shapes = 24,
                            sizes = 3)


map_controls <- rbind(map_controls, none_controls, veg_controls)
map_controls$values
map_controls$values <-factor(map_controls$values,
                             levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap", "nonereg",
                                          "cycle1", "cycle2", "cycle3", "cycle4", "noneregcyc", 
                                          "D1", "D2", "D3", "D4", "D5",
                                          "S1", "S2", "S3", "S4",
                                          "Cycle 1: 2006 – 2009", "Cycle 2: 2010 – 2013", "Cycle 3: 2014 – 2017", "Cycle 4: 2018 – 2020",
                                          "Conifer forest", "Conifer plantation", "Mixed forest", "Hardwood forest", "Mature hardwood forest",
                                          "Successional hardwood forest", "Spruce-fir forest", "Upland forest", "Exotic hardwood forest",
                                          "Forest gap", "Conifer woodland", "Mixed woodland", "Shrubland", "Forested wetland", 
                                          "Shrub wetland", "Open wetland", "Saltmarsh", "Headland", "Intertidal", "Beach", "Subalpine", 
                                          "Open field", "Open water", "Developed", "No data", park_code))

map_controls <- map_controls %>% arrange(values) %>% mutate(fac_order = 1:n())

reg <- joinRegenData(park = park_code, speciesType = "native", canopyForm = 'all', 
                     from = 2016, to = 2019)

reg2 <- reg %>% group_by(Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord) %>% 
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
                        totreg_std2 = ifelse(totreg_std < 0.1, 0.1, totreg_std)
                        )

park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
} else {"+init=epsg:26918"}

coordinates(reg2) <- ~X_Coord + Y_Coord
proj4string(reg2) <- CRS(park_crs)

reg3 <- spTransform(reg2, CRS="+init=epsg:4326")

# writeOGR(reg3, dsn = "./shapefiles", layer = paste0(park_code, "_regen_by_cycle_dd"), 
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)

regen_df <- cbind(reg3@data, long = reg3@coords[,1], lat = reg3@coords[,2])

# regen_df <- regen_df %>% mutate(lat_nudge = case_when(Plot_Number == "017" ~ lat + 0.0001,
                                #                       Plot_Number == "011" ~ lat - 0.0001,
                                #                       TRUE ~ lat), 
                                # long_nudge = case_when(Plot_Number == "017" ~ long + 0.0001,
                                #                        Plot_Number == "011" ~ long - 0.0001,
                                #                        TRUE ~ long),
                                # )
park_veg2 <- fortify(park_veg, region = "veg_type")
park_bound2 <- fortify(park_bound, region = "Park")
View(park_veg2)

par(bg = "#d9d9d9")
map_colors_df <- map_controls %>% filter(group == c("regsize") | values %in% park_veg@data$veg_type) %>% 
                                  select(values, fills) 
map_colors_df

map_colors1 <- t(map_colors_df)
colnames(map_colors1) <- map_colors1[1, ]
map_colors <- map_colors1[-1, ]

map <- ggplot(aes(x = long, y = lat), data = regen_df)+
         geom_polypath(data = park_veg2, 
                       aes(x = long, y = lat, group = group, fill = id), 
                       color = NA, 
                       alpha = 0.8)+
         geom_polygon(data = park_bound2, #fortify(bound, region = "Unit_ID"),
                      aes(x = long, y = lat, group = group),
                      color = "black", size = 1, fill = NA)+
         coord_sf(crs = 4326)+
  geom_spatial_point(data = regen_df %>% filter(totreg_m2 == 0),
                     aes(x = long, y = lat), 
                     shape = 24, fill = "#ff7f00", color = "black",
                     size = 3, crs = 4326)+
  geom_scatterpie(aes(x = long, y = lat, r = totreg_std2/500),
                  data = regen_df %>% filter(totreg_m2>0),
                  cols = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))+
  coord_equal()+
  forestMIDN::theme_FVM()+
  scale_fill_manual(values = map_colors)+
  geom_shadowtext(data = regen_df,
                  aes(label = Plot_Number), color = 'black', bg.color = 'white',
                  check_overlap = TRUE, size = 3,
                  nudge_y = -((regen_df$totreg_std2/500)+0.0004))+
  theme(panel.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        legend.background = element_rect(fill = "#e8e8e8"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.margin = unit(c(0,-0.5,0.5,-0.5),"cm"),
        legend.position = "none"
  )+
  guides(fill = guide_legend(ncol = 2))+
  ggsn::scalebar(fortify(park_bound), dist = 250, dist_unit = 'm',
                 st.dist = 0.05,
                 #location = "bottomright",
                 anchor = c(x = park_bound@bbox[1, "max"]-0.0015,
                            y = park_bound@bbox[2, "min"]-0.005), #0.013
                 st.size = 3,
                 height = 0.025,
                 transform = TRUE,
                 box.color = "#696969",
                 box.fill = c("#d9d9d9","white"),
                 model = "WGS84")+
  ggsn::north(fortify(park_bound), symbol = 12, scale = 0.13,
              anchor = c(x = park_bound@bbox[1, "max"],
                         y = park_bound@bbox[2, "min"]-0.002)) #0.01

map       

regen_leg <- ggplot(aes(x = x, y = y, fill = values, group = values, shape = values), 
                    data = map_controls[map_controls$group == "regsize", ])+
  geom_point()+
  scale_fill_manual(name = "Regeneration size class",
                    values = map_controls$fills[map_controls$group == "regsize"],
                    labels = map_controls$labels[map_controls$group == "regsize"])+
  scale_shape_manual(name = "Regeneration size class",
                     values = map_controls$shapes[map_controls$group == "regsize"], 
                     labels = map_controls$labels[map_controls$group == "regsize"])+
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.1,'cm'),
        panel.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        legend.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        legend.key = element_blank(),
        legend.key.size = unit(0.9, 'line'),
        legend.text = element_text(margin = margin(-0.2,-0.2,0,0), size = 9),
        legend.title = element_text(size = 10),
        plot.margin = unit(c(-2,-2,0,0), "cm"))+
  guides(fill = guide_legend(override.aes = list(size = c(5, 5, 5, 5, 5, 3))))

regen_leg

veg_controls <- map_controls[map_controls$values %in% park_veg@data$veg_type, ]
veg_controls$values <- reorder(veg_controls$values, veg_controls$fac_order) 
veg_controls <- veg_controls %>% arrange(values) 
veg_controls_df <- veg_controls %>% select(values, fills)

veg_colors1 <- t(veg_controls_df)
colnames(veg_colors1) <- veg_colors1[1, ]
veg_colors <- veg_colors1[-1, ]

veg_leg <- ggplot(data = park_veg2, aes(x = long, y = lat, group = group, fill = id))+
  geom_polypath(data = park_veg2, 
                aes(x = long, y = lat, group = group, fill = id), 
                color = NA, alpha = 0.8)+ 
  forestMIDN::theme_FVM()+
  theme(panel.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        legend.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.8,'line'),
        plot.margin = unit(c(-2,-2,0,0), "cm"))+
  scale_fill_manual(values = veg_colors, 
                    name = "Vegetation type", 
                    breaks = veg_controls$breaks)

veg_leg

legend_grid <- plot_grid(
  get_legend(veg_leg), 
  get_legend(regen_leg), ncol = 3,
  rel_widths = c(1,1,4),
  align = 'hv', 
  hjust = 1)
legend_grid

map2 <- map + annotation_custom(ggplotGrob(legend_grid), 
                                xmin = park_bound@bbox[1, "min"]-0.0005,#+0.001,
                                xmax = park_bound@bbox[1, "max"],
                                ymin = park_bound@bbox[2, "min"]-0.021, #-0.028
                                ymax = park_bound@bbox[2, "max"])#+
map2

# ENDED HERE. NEED TO ADD NUDGE_X AND NUDGE_Y FOR PLOTS CLOSE TOGETHER; ALSO NEED TO 
# CREATE A DATA.FRAME TO KEEP TRACK OF THE NUDGE FOR SCALEBAR/NORTH/VEG AND METRIC LEGENDS BY PARK
