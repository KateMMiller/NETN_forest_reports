#----------------------------
library(rgdal)
library(tidyverse)
library(sf)
# park_name = "SARA"

bounds <- readOGR("./shapefiles/NETN_park_bounds.shp", layer = "NETN_park_bounds", 
                  GDAL1_integer64_policy = TRUE)
vegmap <- readOGR("./shapefiles/NETN_vegmap_simplified.shp", 
                 layer = "NETN_vegmap_simplified", GDAL1_integer64_policy = TRUE)

veg_types <- data.frame("veg_type" = sort(unique(vegmap$veg_type)))
#                     beach       confor   confpla    confwood  dev         exohdfor   forgap     forwet
veg_types$fills <- c("#f1ffd7", "#b39267", "#a0725e", "#6a79cb", "#D8D8D8", "#D6887A", "#F2BD66", "#9577a6",
                     #hardfor   headland    intertid  mathdf  mixfor      #mixedwood nodat       ofield
                    "#97BA84", "#FFCFEF", "#2B7B74", "#7ea46b", "#55785e", "#244EAD", "#FFFFFF", "#f5f0b0",
                    #owat       owet        smarsh    shbwet     shrbland  sprufir  subalp
                    "#AFD0F2", "#c497d4", "#37b0a5", "#BB70D9", "#F29839", "#689844", "#E89DCD",
                    #succhd     upfor
                    "#bbdd97", "#7ea46b")
veg_types$labels <- ifelse(veg_types$veg_type == "Developed", paste0("Developed lands"), paste0(veg_types$veg_type))

write.csv(veg_types, "./shapefiles/Vegmap_colors.csv", row.names = FALSE)

# veg_controls = data.frame(values = c(veg_types$veg_type, park_name),
#                           x = 1:26,
#                           y = 1:26,
#                           breaks = c(veg_types$veg_type, park_name),
#                           group = rep("vegmap", 26),
#                           labels = c(veg_types$veg_type, "Park boundary"),
#                           fills = c(veg_types$fill, NA),
#                           shapes = NA,
#                           sizes = NA
# )

regsize_controls = data.frame(values = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"),#, "none"),
                            x = 1:5,
                            y = 1:5,
                            breaks = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"),#, "none"),
                            group = rep("regsize", 5),
                            labels = c("Seedlings 15 \U2013 30 cm", "Seedlings 30 \U2013 100 cm",
                                       "Seedlings 100 \U2013 150 cm", "Seedlings >150 cm", 
                                       "Saplings 1 \U2013 9.9cm DBH"),#, "None present"),
                            fills = c("#D6D6FF", "#8F97E3", "#556CC9", "#244EAD", "#05e636"),#, "#ff7f00"),
                            shapes = c(22, 22, 22, 22, 22),#, 24),
                            sizes = c(8, 8, 8, 8, 8)#, 3)
)

regcyc_controls = data.frame(values = c("cycle1", "cycle2", "cycle3", "cycle4", "noneregcyc"),
                             x = 1:5,
                             y = 1:5,
                             breaks = c("cycle1", "cycle2", "cycle3", "cycle4", "noneregcyc"),
                             group = rep("regcycle", 5),
                             labels = c("Cycle 1: 2006 \U2013 2009", 
                                        "Cycle 2: 2010 \U2013 2013",
                                        "Cycle 3: 2014 \U2013 2017",
                                        "Cycle 4: 2018 \U2013 2020",
                                        "None present"),
                             fills = c("#9fabd7", "#6a79cb", "#49538a", "#05e646", "#ff7f00"),
                             shapes = c(22, 22, 22, 22, 24),
                             sizes = c(8, 8, 8, 8, 3)
                             )

dbi_controls = data.frame(values = c("D1", "D2", "D3", "D4", "D5"),
                      x = 1:5,
                      y = 1:5,
                      breaks = c(1, 2, 3, 4, 5),
                      group = rep("dbi", 5),
                      labels = c("1) No impact: found only in well-maintained deer exclosures.",
                                 "2) Low impact: deer-preferred (DP) species abundant and of varying heights.",
                                 "3) Medium impact: DP species present, but mostly under 30 cm tall. 
                                       DP herbs present but stunted and flowering is uncommon.",
                                 "4) High impact: DP species are rare to absent. Non-preferred and 
                                       browse-resilient vegetation (e.g. beech) limited in height by deer 
                                       browse. DP herbs absent or severely stunted.", 
                                 "5) Very high impact: DP regeneration absent. Non-preferred species 
                                     also reduced by heavy browsing. Distinct deer browse line."),
                      fills = c("#cccccc", "#05e689", "#efdf00", "#f94b24", "#a60808"),
                      shapes = c(21, 21, 21, 21, 21),
                      sizes = c(5, 5, 5, 5, 5)
)

stock_controls = data.frame(values = c("S1", "S2", "S3", "S4"),
                      x = 1:4,
                      y = 1:4,
                      breaks = c(24.9, 49.9, 99.9, 100),
                      group = rep("stock", 4),
                      labels = c("< 25: Severely understocked. Regeneration is insufficient to replace the forest canopy.",
                                 "25 \U2013 50: Moderately stocked regeneration for areas with low deer impacts.",
                                 "50 – 100: Sufficient regeneration to replace forest canopy in areas with low deer impacts.",
                                 "> 100: Sufficiently stocked with regeneration for areas with high deer impacts."),
                      fills = c("#a60808", "#f94b24", "#efdf00", "#05e689"),
                      shapes = c(21, 21, 21, 21),
                      sizes = c(3, 4, 5, 6)
)

invcycle_controls = data.frame(values = c("Cycle 1: 2006 \U2013 2009", 
                                        "Cycle 2: 2010 \U2013 2013",
                                        "Cycle 3: 2014 \U2013 2017",
                                        "Cycle 4: 2018 \U2013 2020"),
                             x = 1:4,
                             y = 1:4,
                             breaks = c("Cycle 1: 2006 \U2013 2009", 
                                        "Cycle 2: 2010 \U2013 2013",
                                        "Cycle 3: 2014 \U2013 2017",
                                        "Cycle 4: 2018 \U2013 2020"),
                             group = rep("invcycle", 4),
                             labels = c("Cycle 1: 2006 \U2013 2009", 
                                        "Cycle 2: 2010 \U2013 2013",
                                        "Cycle 3: 2014 \U2013 2017",
                                        "Cycle 4: 2018 \U2013 2020"),
                             fills = c("#9fabd7", "#6a79cb", "#49538a", "#05e646"),
                             shapes = c(22, 22, 22, 22),
                             sizes = c(8, 8, 8, 8)
)

map_controls <- rbind(regsize_controls, regcyc_controls, dbi_controls, stock_controls, invcycle_controls)

#write.csv(map_controls, "./shapefiles/map_controls.csv", row.names = FALSE)

pie_expfac1 <- data.frame(park_code = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"),
                          pie_expfac = c(700, 50, 80, 80, 80, 25, 90, 22)) #~trial and error based on park's ideal map scale  


# controls = data.frame(values = c(),
#                       x = ,
#                       y = ,
#                       breaks = c(),
#                       group = rep(),
#                       labels = c(),
#                       fills = c(),
#                       shapes = c(),
#                       sizes = c()
# )
