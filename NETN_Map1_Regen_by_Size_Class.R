library(forestNETN)
library(tidyverse)
library(rgdal)
library(sf)
library(scatterpie)
library(ggpolypath)
#library(ggrepel)
library(shadowtext)
library(ggspatial)
library(rgeos)
library(cowplot)
#library(ggpubr) #ggarrange
#library(grid)
#library(ggsn) # for scalebar and north
#library(ggmap)
#library(leaflet.minicharts)
#library(leaflet)
#library(htmltools)
#library(maptools)
importData()

park_code = "MABI"
park_long_name = "Marsh-Billings-Rockefeller National Historical Park"
park_crs <- if(park_code %in% c("ACAD", "MIMA")){"+init=epsg:26919"
                                         } else {"+init=epsg:26918"}

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

reg2$totreg_std <- (reg2$totreg_m2 - min_totreg) / (diff_totreg)

coordinates(reg2) <- ~X_Coord + Y_Coord
proj4string(reg2) <- CRS(park_crs)

reg3 <- spTransform(reg2, CRS="+init=epsg:4326")
str(reg3@coords)

writeOGR(reg3, dsn = "./shapefiles", layer = paste0(park_code, "_regen_by_cycle_dd"), 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

regen <- readOGR("./shapefiles/MABI_regen_by_cycle_dd.shp", 
                 layer = "MABI_regen_by_cycle_dd", GDAL1_integer64_policy = TRUE)

regen_df <- cbind(reg3@data, long = reg3@coords[,1], lat = reg3@coords[,2])

veg_shp <- readOGR("./shapefiles/MABI_vegmap_diss.shp", 
                 layer = "MABI_vegmap_diss", GDAL1_integer64_policy = TRUE)

veg_dd <- spTransform(veg_shp, CRS="+init=epsg:4326")

writeOGR(veg_dd, dsn = "./shapefiles", layer = paste0(park_code, "_vegmap_dd"),
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

veg_shp <- readOGR("./shapefiles/MABI_vegmap_dd.shp", layer = "MABI_vegmap_dd", 
                   GDAL1_integer64_policy = TRUE)

bound <- readOGR("./shapefiles/MABI_Boundary_2002_dd.shp", 
                 layer = "MABI_Boundary_2002_dd", GDAL1_integer64_policy = TRUE)

regen_df <- regen_df %>% mutate(lat_nudge = case_when(Plot_Number == "017" ~ lat + 0.0001,
                                                      Plot_Number == "011" ~ lat - 0.0001,
                                                      TRUE ~ lat), 
                                long_nudge = case_when(Plot_Number == "017" ~ long + 0.0001,
                                                       Plot_Number == "011" ~ long - 0.0001,
                                                       TRUE ~ long),
                                totreg_std2 = ifelse(totreg_std <0.1, 0.1,totreg_std),
                                )

table(veg_shp@data$Map_Group)

regen_colors = c("sd15_30" = "#D6D6FF",
                 "sd30_100" = "#8F97E3",
                 "sd100_150" = "#556CC9",
                 "sd150p" = "#244EAD",
                 "sap" = "#05e636")

veg_colors = c("Conifer forest" = "#a0725e",
               "Conifer plantation" = "#b39267",
               "Developed" = "#c2c2c2",
               "Forested wetland" = "#9577a6",
               "Hardwood forest" = "#7ea46b",
               "Mixed forest" = "#55785e",
               "Open field" = "#f5f0b0",
               "Open water" = "#9bd2ef",
               "MABI" = NA)

none_colors = c("MABI" = "#ff7f00")

map_colors = c(regen_colors, veg_colors, none_colors)


veg_shp2 <- fortify(veg_shp, region = "Map_Group")
bound2 <- fortify(bound, region = "Unit_ID")

regen_controls = data.frame(values = ordered(c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap", "MABI"),
                                           levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap", "MABI")),
                            x = 1:6,
                            y = 1:6,
                            breaks = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap", "MABI"),
                            group = rep("reg", 6),
                            labels = c("Seedlings 15 \U2013 30 cm", "Seedlings 30 \U2013 100 cm",
                                     "Seedlings 100 \U2013 150 cm", "Seedlings >150 cm", 
                                     "Saplings 1 \U2013 9.9cm DBH", "None present"),
                            fills = c("#D6D6FF", "#8F97E3", "#556CC9", "#244EAD", "#05e636", "#ff7f00"),
                            shapes = c(22, 22, 22, 22, 22, 24),
                            sizes = c(8, 8, 8, 8, 8, 3)
)

veg_controls = data.frame(values = ordered(c("Conifer forest", "Conifer plantation", "Hardwood forest", 
                                             "Mixed forest","Forested wetland",
                                             "Developed", "Open field", "Open water", "MABI"),
                                           levels = c("Conifer forest", "Conifer plantation", "Hardwood forest", 
                                                      "Mixed forest","Forested wetland",
                                                      "Developed", "Open field", "Open water", "MABI")),
                          x = 7:15,
                          y = 7:15,
                          breaks = c("Conifer forest", "Conifer plantation", "Hardwood forest", 
                                     "Mixed forest","Forested wetland",
                                     "Developed", "Open field", "Open water", "MABI"),
                          group = rep("veg", 9),
                          labels = c("Conifer forest", "Conifer plantation", "Hardwood forest", 
                                     "Mixed forest","Forested wetland",
                                     "Developed Lands", "Open field", "Open water", "Park Boundary"),
                          fills = c("#a0725e", "#b39267", "#7ea46b", "#55785e", "#9577a6", "#c2c2c2", 
                                    "#f5f0b0", "#9bd2ef", NA),
                          shapes = NA,
                          sizes = NA
                          )
                                           
map_controls = rbind(regen_controls, veg_controls)

head(veg_shp2)

map <-   ggplot(aes(x = long_nudge, y = lat_nudge), data = regen_df) +
         geom_polypath(data = veg_shp2, 
                      aes(x = long, y = lat, group = group, fill = id), 
              color = NA, alpha = 0.8)+
         geom_polygon(data = fortify(bound, region = "Unit_ID"), 
                      aes(x = long, y = lat, group = group, fill = id), 
                          color = "black", size = 1)+
         coord_sf(crs = 4326)+
         geom_spatial_point(data = regen_df %>% filter(totreg_m2==0),
                            aes(x = long, y = lat, group = Unit_Code),
                    shape = 24, fill = "#ff7f00", color = 'black', size = 3,
                    crs = 4326)+
         geom_scatterpie(aes(x = long_nudge, y = lat_nudge, r = totreg_std2/500),
                    data = regen_df %>% filter(totreg_m2>0),
                    cols = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))+
         coord_equal()+
         forestMIDN::theme_FVM() +
         scale_fill_manual(values = map_colors)+
         geom_shadowtext(data = regen_df,
                         aes(label = Plot_Number), color = 'black', bg.color = 'white',
                         check_overlap = TRUE,
                         nudge_y = -((regen_df$totreg_std2/500)+0.0003))+

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
               plot.margin = unit(c(1,0.2,1,0.2),"cm"),
               legend.position = "none"
               )+
         guides(fill = guide_legend(ncol = 2))+
         ggsn::scalebar(fortify(bound), dist = 250, dist_unit = 'm',
                        st.dist = 0.05,
                        anchor = c(x = bound@bbox[1, "max"],
                                   y = bound@bbox[2, "min"]-0.01),
                        st.size = 4,
                        height = 0.05,
                        transform = TRUE,
                        box.color = "#696969",
                        box.fill = c("#d9d9d9","white"),
                        model = "WGS84")+
          ggsn::north(fortify(bound), symbol = 12, scale = 0.15,
                      anchor = c(x = bound@bbox[1, "max"]+0.002,
                                 y = bound@bbox[2, "min"]-0.007))
map
#####

regen_leg <- ggplot(aes(x = x, y = y, fill = values, group = values, shape = values), 
                    data = regen_controls)+
             geom_point()+
             scale_fill_manual(name = "Regeneration size class",
                               values = map_controls$fills,
                               labels = map_controls$labels)+
             scale_shape_manual(name = "Regeneration size class",
                                values = map_controls$shapes, 
                                labels = map_controls$labels)+
             theme(legend.position = "bottom",
                   legend.direction = "vertical",
                   legend.spacing.y = unit(0.1,'cm'),
                   panel.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   legend.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   legend.key = element_blank(),
                   legend.text = element_text(margin = margin(-0.2,-0.2,0,0)),
                   plot.margin = unit(c(-2,-2,0,0), "cm"))+
             guides(fill = guide_legend(override.aes = list(size = c(8,8,8,8,8,3))))
             
regen_leg

veg_leg <- ggplot(data = veg_shp2, aes(x = long, y = lat, group = group, fill = id))+
             geom_polypath(data = veg_shp2, 
                aes(x = long, y = lat, group = group, fill = id), 
                color = NA, alpha = 0.8)+ 
             forestMIDN::theme_FVM()+
             theme(panel.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   plot.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   legend.background = element_rect(fill = "#e8e8e8", color = "#e8e8e8"),
                   plot.margin = unit(c(-2,-2,0,0), "cm"))+
             scale_fill_manual(values = veg_colors, 
                               name = "Vegetation type", 
                               breaks = veg_controls$breaks)

legend_grid <- plot_grid(
                 get_legend(veg_leg), 
                 get_legend(regen_leg), ncol = 3,
                 rel_widths = c(1,1,2),
                 align = 'hv', 
                 hjust = 1)
legend_grid

map2 <- map + annotation_custom(ggplotGrob(legend_grid), 
                        xmin = bound@bbox[1, "min"]-0.001,
                        xmax = bound@bbox[1, "max"],
                        ymin = bound@bbox[2, "min"]-0.028,
                        ymax = bound@bbox[2, "max"])

map2
ggsave("MABI_regen_size_class.jpg", map2, dpi = 300, width = 7.5, height = 5.5, units = 'in')
ggsave("MABI_test.jpg", map, dpi=300, width = 7.5, units = 'in')

?ggsave
