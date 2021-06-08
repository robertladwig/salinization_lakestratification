library(ggspatial)
library(sf)
library(tidyverse)
library(NTLlakeloads) # devtools::install_github('hdugan/NTLlakeloads')
library(patchwork)
library(lubridate)

# ESRI base map for plotting
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Lake shapefiles and bathymetry can be found at 
# Center for Limnology and NTL LTER. 2013. North Temperate Lakes LTER Yahara Lakes District Bathymetry ver 9. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/bfc7fcc762572c72e2d2d35cab1d7662 (Accessed 2021-06-08).
# Center for Limnology and NTL LTER. 2013. North Temperate Lakes LTER Yahara Lakes District Study Lakes ver 10. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/7b0a93bd469d6072cc22ec3355483df8 (Accessed 2021-06-08).
lakes.S = st_read('NTL_data/ntl152_v2_0/') %>% filter(LAKEID %in% c('ME', 'MO'))
bathyME = st_read('NTL_data/ntl153_v3_0/mendota-contours-all.shp') %>% 
  st_set_crs(3071)
bathyMO = st_read('NTL_data/ntl153_v3_0/monona_bathy.shp') %>% 
  st_set_crs(3071)

# sampling locations for manual sampling and buoy deployment
# Lake Mendota	43.09885,-89.40545
# Lake Monona	43.06337,-89.36086
# Manually build a dataframe 
sampling.sites = data.frame(site = c('ME','MO'),lat = c(43.09885, 43.06337), lon = c(-89.40545,-89.36086))
sampling.sites.sf = st_as_sf(sampling.sites, coords = c("lon", "lat"), 
                      crs = 4326)

mapS = ggplot(lakes.S) +
  annotation_map_tile(type = world_gray, zoom = 14) +
  # geom_sf(data = lakes.S, fill = alpha('lightsteelblue1',1), size = 0.2) +
  geom_sf(data = bathyME, fill = alpha('#bfd9e0',1), color = 'grey50', size = 0.1) +
  geom_sf(data = bathyMO, fill = alpha('#bfd9e0',1), color = 'grey50', size = 0.1) +
  geom_sf(data = sampling.sites.sf, aes(fill = site), size = 1, shape = 21, show.legend = FALSE) +
  scale_fill_manual(values = c('#bfd9e0', '#cfd160')) +
  # geom_sf_label(data = lakes.S, aes(label = SHAIDNAME), label.size = 0.1, alpha = 0.7, size = 2) +
  annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.05,'in'), text_cex = 0.6) + # Scale bar
  annotation_north_arrow(location = "tr", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.2,'in'), width = unit(0.4,'in'),
                         style = north_arrow_minimal) + # North Arrow
  theme_bw(base_size = 6) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1)) +
  # coord_sf(xlim = c(-89.71, -89.58), ylim = c(45.99, 46.04)) +
  NULL

ggsave(plot = mapS, 'NTL_data/Figures/Map_Mendota_Monona.png', width = 3, height = 3, dpi = 500, bg = "transparent")

##### Chloride time series ########
ions = loadLTERions() %>%  filter(lakeid %in% c('ME','MO')) # Load LTER ion data 

# Mendota chloride before 1995
me.dnr = read_csv('NTL_data/LakeMendota_Dane_WI_VIII.csv') %>% mutate(lakeid = 'ME') %>% 
  bind_rows(read_csv('NTL_data/LakeMonona_Dane_WI_VIII.csv') %>%  mutate(lakeid = 'MO')) %>% 
  mutate(year4 = year(Sample.Date)) %>% 
  group_by(year4, lakeid) %>% 
  dplyr::summarise(mean.cl = mean(Chloride_mgL, na.rm = T)) %>% 
  mutate(sampledate = as.Date(paste0(year4,'-07-01')))

me.cl = ions %>% filter(lakeid %in% c('ME','MO')) %>% 
  filter(!is.na(cl))

me.cl.mean = me.cl %>% group_by(lakeid, year4) %>% 
  dplyr::summarise(mean.cl = mean(cl, na.rm = T)) %>% 
  mutate(sampledate = as.Date(paste0(year4,'-07-01')))

cl1.mean = ggplot(me.dnr) +
  geom_point(aes(x = sampledate, y = mean.cl, group = lakeid, fill = lakeid), shape = 21) +
  geom_point(data = me.cl.mean, aes(x = sampledate, y = mean.cl, group = lakeid, fill = lakeid), shape = 21) +
  scale_fill_manual(values = c('#bfd9e0', '#cfd160'), name = 'Lake', labels = c('Mendota','Monona')) +
  ylab(bquote('Chloride' ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank(), 
        legend.text = element_text(size = 6),
        legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black"),
        legend.key.height =  unit(0.2,"cm"), 
        legend.key.width =  unit(0.2,"cm"), 
        legend.position=c(.2,.8)); cl1.mean
ggsave(plot = cl1.mean, 'NTL_data/Figures/LongTermCL.pdf', width = 3, height = 2, dpi = 500, bg = "transparent")


cl.profiles = me.cl %>% filter(month(sampledate) <= 3) %>% 
  filter(year4 > 2000) %>% 
  mutate(lakename = if_else(lakeid == 'ME', 'Lake Mendota', 'Lake Monona')) %>% 
  ggplot(.) +
  geom_path(aes(x = cl, y = depth, group = year4), color = 'grey60', alpha = 0.5, size = 0.2) +
  geom_point(aes(x = cl, y = depth, fill = lakeid), shape = 21) +
  scale_fill_manual(values = c('#bfd9e0', '#cfd160'), name = 'Lake', labels = c('Mendota','Monona')) +
  scale_color_manual(values = c('#bfd9e0', '#cfd160'), name = 'Lake', labels = c('Mendota','Monona')) +
  scale_y_reverse() +
  facet_wrap(~lakename, scales = 'free_x') +
  ylab('Depth (m)') +
  xlab(bquote('Chloride' ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none')
ggsave(plot = cl.profiles, 'NTL_data/Figures/NTLprofiles.pdf', width = 3, height = 3, dpi = 500, bg = "transparent")


mapS + cl1.mean +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('NTL_data/Figures/Map_Mendota_Monona_Cl.mean.png', width = 6.5, height = 4, dpi = 500, bg = "transparent")

layout <- "
AAABBB
AAABBB
CCCCCC
"
mapS + cl.profiles + cl1.mean +
  plot_layout(design = layout)  +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('NTL_data/Figures/Map_Trend_Profiles.png', width = 6.5, height = 5, dpi = 500, bg = "transparent")


###### Density as a function of temperature #######
# For statistics in introduction

# Freshwater
calc_dens <- function(wwtemp){
  dens = 999.842594 + (6.793952 * 10^-2 * wwtemp) - (9.095290 * 10^-3 *wwtemp^2) + (1.001685 * 10^-4 * wwtemp^3) - (1.120083 * 10^-6* wwtemp^4) + (6.536336 * 10^-9 * wwtemp^5)
  return(dens)
}

df = data.frame(temp = 0:20) %>% 
  mutate(dens = calc_dens(temp)) 
df %>% filter(temp == 10 | temp == 20) %>% 
  mutate(diff = diff(dens))

# Saltwater 
# Density of saltwater at 20 °C and 1 dbar
library(gsw)
df2 = data.frame(salinity = 0:35) %>% 
  mutate(dens = gsw_rho_t_exact(salinity, 20, 1))
df2 %>% filter(salinity == 0 | salinity == 2) %>% 
  mutate(diff = diff(dens))


  
