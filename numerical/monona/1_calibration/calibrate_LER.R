setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("GLM", "GOTM", "Simstrat")


### (2) Calibration:
export_config(config_file = config_file, model = model)
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

library(parallel)
ncores <- length(model)
clust <- parallel::makeCluster(ncores, setup_strategy = "sequential")
clusterExport(clust, varlist = list("config_file"),
              envir = environment())
clusterEvalQ(clust, library(LakeEnsemblR))
resMCMC <- parLapply(clust, model, function(m) {
   cali_ensemble(config_file = config_file, 
                 num = 5000, 
                 cmethod = "LHC",
                 model = m, parallel = TRUE)
})
stopCluster(clust)

save(resMCMC, file = 'cal_LHC_5000_May10.RData')

resMCMC[[1]]$GLM$bestpar
pairs(resMCMC[[1]]$GLM)

resMCMC[[2]]$GOTM$bestpar
pairs(resMCMC[[2]]$GOTM)

resMCMC[[3]]$Simstrat$bestpar
pairs(resMCMC[[3]]$Simstrat)

res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resMCMC))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)


### (2) Use calibrated parameters:

export_config(config_file = config_file, model = model)
write(glm_ice_fix,file="GLM/glm3.nml",append=TRUE)

run_ensemble(config_file = config_file, model = model)

ncdf <- 'output/ensemble_output.nc'
calc_fit(ncdf = ncdf, model = model)
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


plist <- plot_resid(ncdf = ncdf,var = "temp",
                    model = model)
p <- ggarrange(plist$obs_res + theme_light(),
               plist$yday_res + theme_light(), 
               plist$res_depth + theme_light(),
               plist$res_dist + theme_light(), ncol = 2, nrow = 2); p
ggsave('output/diagnostics.png', p,  dpi = 300,width = 400,height = 200, units = 'mm')

out_res <- analyse_ncdf(ncdf = ncdf,
                        model = model)
# look at returned values
names(out_res)

print(out_res[["stats"]])
write.csv(out_res[["stats"]], file = 'output/stats.csv')
print(out_res[["strat"]])

ggplot(out_res[["strat"]], aes(year, TotIceDur, col = model)) + geom_line() + geom_point() 


