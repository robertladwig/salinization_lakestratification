# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

file.copy(c('../../1_calibration/LakeEnsemblR_bathymetry_standard.csv',
            '../../1_calibration/LakeEnsemblR_inflow_standard.csv',
            '../../1_calibration/LakeEnsemblR_initial_standard.csv',
            '../../1_calibration/LakeEnsemblR_meteo_standard.csv',
            '../../1_calibration/LakeEnsemblR_wtemp_profile_standard.csv',
            '../../1_calibration/LakeEnsemblR.yaml'), '.', overwrite = T)

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("GLM", "GOTM", "Simstrat")

input_yaml_multiple(file = config_file, value = "1995-01-01 00:00:00",
                    key1 = "time", key2 = "start")
input_yaml_multiple(file = config_file, value = "2015-12-30 00:00:00",
                    key1 = "time", key2 = "stop")

library(lubridate)
inflow.df <- read.csv('LakeEnsemblR_inflow_standard.csv')
inflow.df$Salinity_practicalSalinityUnits <- 0.1
months <- month(inflow.df$datetime)
idx <- which(months %in% c(12, 1, 2, 3, 4))
inflow.df$Salinity_practicalSalinityUnits[idx] <- 4.5

write.csv(inflow.df, file = 'LakeEnsemblR_inflow_scenario1.csv',quote = F,row.names = F)

input_yaml_multiple(file = config_file, value = "LakeEnsemblR_inflow_scenario1.csv",
                    key1 = "inflows", key2 = "file")

export_config(config_file = config_file, model = model)
file.copy(from = '../salt_cond.dat', to = 'GOTM/', overwrite = T)
input_yaml_multiple(file = 'GOTM/gotm.yaml', value = "salt_cond.dat",
                    key1 = "salinity", key2 = "file")
input_yaml_multiple(file = 'GOTM/gotm.yaml', value = 2,
                    key1 = "salinity", key2 = "method")
glm_ice_fix <- "&snowice
   snow_albedo_factor = 1.0    //# scaling multiplier for computed albedo
   snow_rho_max       = 300.   //# maximum snow density allowed
   snow_rho_min       = 50.    //# minimum snow density allowed
   snow_water_equivalent = 0.1 //# snow volume to water equivalent, 10:1
   snow_rain_compact = 1.      //# set at module level
   K_ice_white = 2.3           //# thermal conductivity of white ice
   K_ice_blue = 2.0            //# thermal conductivity of blue ice
   K_water = 0.57              //# molecular thermal conductivity of water
   f_sw_wl1 = 0.7              //# fraction of short wave radiation in first wavelength band
   f_sw_wl2 = 0.3              //# fraction of short wave radiation in second wavelength band
   attn_ice_blue_wl1 = 1.5     //# attenuation coefficient of the ice in the first spectral band
   attn_ice_blue_wl2 = 20.     //# attenuation coefficient of the ice in the second spectral band
   attn_ice_white_wl1 = 6.0    //# attenuation coefficient of the white ice in the first spectral band
   attn_ice_white_wl2 = 20.    //# attenuation coefficient of the white ice in the second spectral band
   attn_snow_wl1 = 6.0         //# attenuation coefficient of the snow in the first spectral band
   attn_snow_wl2 = 20.         //# attenuation coefficient of the snow in the second spectral band
   rho_ice_blue = 917.0        //# density of blue ice
   rho_ice_white = 890.0       //# density of white ice
   min_ice_thickness = 0.05    //# threshold thickness for new ice-on, or ice-off
   dt_iceon_avg = 0.01          //# moving average time-scale of water temp to identify ice-on transition
/
&sediment
   benthic_mode = 2
   n_zones = 2
   zone_heights = 10, 25
   sed_temp_mean = 5.05, 13.5
   sed_temp_amplitude = 4, 3
   sed_temp_peak_doy = 230, 230
   sed_heat_Ksoil = 0.1, 0.1
/
"

write(glm_ice_fix,file="GLM/glm3.nml",append=TRUE)

run_ensemble(config_file = config_file, model = model)

ncdf <- 'output/ensemble_output.nc'
fit_analytics <- calc_fit(ncdf = ncdf, model = model)
write.csv(fit_analytics, file = 'output/fit.csv')

# Load libraries for post-processing
library(gotmtools)
library(ggplot2)
library(ggpubr)

## Plot model output using gotmtools/ggplot2
# Extract names of all the variables in netCDF
ncdf <- 'output/ensemble_output.nc'
vars <- gotmtools::list_vars(ncdf)
vars # Print variables

p1 <- plot_heatmap(ncdf)
p1
# Change the theme and increase text size for saving
p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(limits = c(0, 31),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))); p1
# Save as a png file
ggsave('output/ensemble_wtmp.png', p1,  dpi = 300,width = 12, height = 6 )#384,height = 280, units = 'mm')

p1 <- plot_heatmap(ncdf, var = 'dens')
p1
# Change the theme and increase text size for saving
p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))); p1
# Save as a png file
ggsave('output/ensemble_dens.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')


p1 <- plot_heatmap(ncdf, var = 'salt')
p1
# Change the theme and increase text size for saving
p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))); p1
# Save as a png file
ggsave('output/ensemble_salt.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')


p1 <- plot_ensemble(ncdf, model = model,
                    var = "temp", depth = 2,
                    residuals = TRUE)
# Arrange the two plots above each other
p <- ggarrange(p1[[1]] + theme_light(),
          p1[[2]] + theme_light(), ncol = 1, nrow = 2); p
ggsave('output/ensemble_ts2m.png', p,  dpi = 300,width = 384,height = 180, units = 'mm')

p1 <- plot_ensemble(ncdf, model = model,
                    var = "temp", depth = 22,
                    residuals = TRUE)
# Arrange the two plots above each other
p <- ggarrange(p1[[1]] + theme_light(),
               p1[[2]] + theme_light(), ncol = 1, nrow = 2); p
ggsave('output/ensemble_ts22m.png', p,  dpi = 300,width = 384,height = 180, units = 'mm')

p1 <- plot_ensemble(ncdf, model = model,
                    var = "ice_height")
# Arrange the two plots above each other
p <- p1  + theme_light(); p
ggsave('output/ensemble_iceheight.png', p,  dpi = 300,width = 384,height = 180, units = 'mm')


plist <- plot_resid(ncdf = ncdf,var = "temp",
                    model = model)
p <- ggarrange(plist$obs_res + theme_light(),
               plist$yday_res + theme_light(), 
               plist$res_depth + theme_light(),
               plist$res_dist + theme_light(), ncol = 2, nrow = 2); p
ggsave('output/diagnostics.png', p,  dpi = 300,width = 400,height = 200, units = 'mm')

## Calculate Schmidt Stability using rLakeAnalyzer
out <- load_var(ncdf = "output/ensemble_output.nc", var = "temp")
bathy <- read.csv('LakeEnsemblR_bathymetry_standard.csv')
colnames(bathy) <- c("depths", "areas")
ts.sch <- lapply(out, function(x) {
   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})
## Reshape to data.frame
df <- reshape2::melt(ts.sch, id.vars = 1)
colnames(df)[4] <- "model"
## plot results
g <- ggplot(df, aes(datetime, value, colour = model)) +
   geom_line() +
   labs(y = "Schmidt stability (J/m2)") +
   theme_light(); g
ggsave('output/ensemble_schmidt.png', g,  dpi = 300,width = 384,height = 180, units = 'mm')