library(ggspatial)
library(sf)
library(ggrepel)
library(tidyverse)
library(NTLlakeloads)
library(patchwork)
library(lubridate)

# esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
# esri_streets <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Street_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')

# lakes
lakes.S = st_read('chlorideData/ntl152_v2_0/') %>% filter(LAKEID %in% c('ME', 'MO'))
bathyME = st_read('chlorideData/ntl153_v3_0/mendota-contours-all.shp') %>% 
  st_set_crs(3071)
bathyMO = st_read('chlorideData/ntl153_v3_0/monona_bathy.shp') %>% 
  st_set_crs(3071)

# sampling locations
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

ggsave(plot = mapS, 'chlorideData/Map_Mendota_Monona.png', width = 3, height = 3, dpi = 500, bg = "transparent")

##### Chloride ########
ions = loadLTERions() %>%  filter(lakeid %in% c('ME','MO'))

# knb-lter-ntl.319.17 Mendota chloride before 1995
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/319/17/ada50bfcdf3672cb145ab6ba0a4d75d1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

me.dnr = read_csv(infile1) %>% filter(lakeid %in% c('ME','MO')) %>% 
  group_by(year4, lakeid) %>% 
  dplyr::summarise(mean.cl = mean(cl, na.rm = T)) %>% 
  mutate(sampledate = as.Date(paste0(year4,'-07-01')))

# Mendota chloride before 1995
me.dnr = read_csv('chlorideData/LakeMendota_Dane_WI_VIII.csv') %>% mutate(lakeid = 'ME') %>% 
  bind_rows(read_csv('chlorideData/LakeMonona_Dane_WI_VIII.csv') %>%  mutate(lakeid = 'MO')) %>% 
  mutate(year4 = year(Sample.Date)) %>% 
  group_by(year4, lakeid) %>% 
  dplyr::summarise(mean.cl = mean(Chloride, na.rm = T)) %>% 
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
ggsave(plot = cl1.mean, 'chlorideData/LongTermCL.pdf', width = 3, height = 2, dpi = 500, bg = "transparent")


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
ggsave(plot = cl.profiles, 'chlorideData/NTLprofiles.pdf', width = 3, height = 3, dpi = 500, bg = "transparent")


mapS + cl1.mean +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('chlorideData/Map_Mendota_Monona_Cl.mean.png', width = 6.5, height = 4, dpi = 500, bg = "transparent")

layout <- "
AAABBB
AAABBB
CCCCCC
"
mapS + cl.profiles + cl1.mean +
  plot_layout(design = layout)  +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('chlorideData/Map_Trend_Profiles.png', width = 6.5, height = 5, dpi = 500, bg = "transparent")


###### Density as a function of temperature #######
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


  
