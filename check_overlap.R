#----------------------
# Function to eventually add to forestNETN/MIDN
# Internal function used within nudge_XY
#   - There's still a check of overlap I'm missing- if a buffer is inside a plot only the inside plot is caught
#----------------------
# ID plots that have at least one overlapping pie. Used within nudge_XY
check_overlap <- function(sf){
  # Create buffer approximately the size of pies. 
  sf_buff <- st_buffer(sf, sf$fig_radius)
  
  # Determine pies that overlap
  overlaps1 <- st_drop_geometry(st_intersection(sf_buff)) %>% group_by(Plot_Name) %>%
    summarize(num_overlaps = sum(!is.na(Plot_Name))-1,
              .groups = "drop") %>% ungroup() %>%
    filter(num_overlaps > 0) %>% select(Plot_Name) %>% mutate(type = 'overlap1')

  overlaps2 <- st_overlaps(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(overs = length(x))) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(overs > 0) %>% select(Plot_Name) %>% mutate(type = 'overlap2')
  
  # Determine pies that are within another pie
  withins <- st_within(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(inside = length(x)-1)) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(inside > 0) %>% select(Plot_Name) %>% mutate(type = 'within')
  
  covers <- st_covers(sf_buff) %>% set_names(plot_list) %>% 
    lapply(FUN = function(x) data.frame(covers = length(x)-1)) %>% 
    bind_rows() %>% mutate(Plot_Name = plot_list) %>% 
    filter(covers > 0) %>% select(Plot_Name) %>% mutate(type = 'covers')
  
  # Bind plot list of pies to shift coords
  plots_to_shift <- rbind(overlaps1, overlaps2, withins, covers) %>% arrange(Plot_Name, type) %>% 
    select(Plot_Name) %>% unique()
  
  return(plots_to_shift)
}