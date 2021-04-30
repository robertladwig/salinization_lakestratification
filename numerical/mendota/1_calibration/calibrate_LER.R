setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Copy template folder
# template_folder <- "/Users/robertladwig/Documents/AEMONJ/LakeEnsemblR/inst/extdata/mendota" #system.file("extdata/feeagh", package= "LakeEnsemblR")
# dir.create("example") # Create example folder
# file.copy(from = template_folder, to = "example", recursive = TRUE)
setwd("example/mendota") # Change working directory to example folder

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("GLM", "GOTM", "Simstrat")

# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
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

#input_yaml_multiple(file = "GOTM/gotm.yaml", value = 2.0,
#                       key1 = "g2", key2 = "constant_value")#, key3 = "file")

cali_result_MCMC <- cali_ensemble(config_file = config_file, 
                             num = 5000, 
                             cmethod = "MCMC",
                             model = model, parallel = TRUE)

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
# stop cluster
stopCluster(clust)

save(resMCMC, file = 'cal_LHC_5000_April13.RData')

resMCMC[[1]]$GLM$bestpar
pairs(resMCMC[[1]]$GLM)

resMCMC[[2]]$GOTM$bestpar
pairs(resMCMC[[2]]$GOTM)

resMCMC[[3]]$Simstrat$bestpar
pairs(resMCMC[[3]]$Simstrat)


res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(resMCMC))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)


resMCMC[["GLM"]][["bestpar"]]
# # wind_speed                 swr mixing/coef_mix_hyp 

resMCMC[["GOTM"]][["bestpar"]]
# # wind_speed              swr turb_param/k_min 

resMCMC[["Simstrat"]][["bestpar"]]
# wind_speed          swr     a_seiche
save(cali_result_MCMC, file = 'cal_MCMC_1000_Feb06.RData')

load('cal_MCMC_1000_Jan11.RData')

cali_result_LHC <- cali_ensemble(config_file = config_file, 
                             num = 1000, 
                             cmethod = "LHC",
                             model = model, parallel = TRUE)
save(cali_result_LHC, file = 'cal_MCMC_1000_Jan09_LHC.RData')


res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(cali_result_LHC))
# 

load('cal_MCMC_1000_Feb06.RData')
# get the best parameter set for each model using rmse
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)

# 
# cali_result <- cali_ensemble(config_file = config_file, 
#                              num = 1000, 
#                              cmethod = "LHC",
#                              model = c('GLM', "GOTM"), parallel = TRUE)
# 
# # load results of LHC in a data.frame
# res_LHC <- load_LHC_results(config_file = config_file, model = c("GLM", "GOTM"), res_files = unlist(cali_result))
# 
# # get the best parameter set for each model using rmse
# best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
# print(best_p)
# 
# save(cali_result, file = 'cal_MCMC_1000_GLMGOTM_Jan08.RData')
# 
# cali_result <- cali_ensemble(config_file = config_file, 
#                              num = 1000, 
#                              cmethod = "LHC",
#               model = model, parallel = TRUE)
# 
# save(cali_result, file = 'cal_MCMC_1000_Jan08.RData')
# 
# load('cal_MCMC_1000_Jan08.RData')
# 
# # load results of LHC in a data.frame
# res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(cali_result))
# 
# # get the best parameter set for each model using rmse
# best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
# print(best_p)
# 
# # we can plot the results of the latin hypercube calibration for e.g. GOTM using plot_LHC
# plot_LHC(config_file = config_file, model = "GOTM", res_files = cali_result$GOTM,
#          qual_met = "nse")
# # or we can plot it for all models using
# plot_LHC(config_file = config_file, model = model, res_files = unlist(cali_result),
#          qual_met = "nse", best = "high")
# 
# ncdf <- 'output/ensemble_output.nc'

# calc_fit(ncdf = ncdf, model = model)

# cali_result[["GLM"]][["bestpar"]]
# # wind_speed                 swr mixing/coef_mix_hyp 
# # 1.0493695           0.9150658           0.3671161 
# cali_result[["GOTM"]][["bestpar"]]
# # wind_speed              swr turb_param/k_min 
# # 9.634807e-01     1.039566e+00     7.901281e-07 
# cali_result[["Simstrat"]][["bestpar"]]x
# # wind_speed          swr     a_seiche 
# # 1.001248e+00 9.599311e-01 9.963318e-05 


# cali_res <- cali_ensemble(config_file = config_file, num = 1000, 
#                           cmethod = "LHC",
#                           parallel = TRUE, 
#                           model = model)
# 
# # load results of LHC in a data.frame
# res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(cali_res))
# 
# # get the best parameter set for each model using rmse
# best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
# print(best_p)


export_config(config_file = config_file, model = model)
write(glm_ice_fix,file="GLM/glm3.nml",append=TRUE)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model)

ncdf <- 'output/ensemble_output.nc'
calc_fit(ncdf = ncdf, model = model)

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
print(out_res[["strat"]])

ggplot(out_res[["strat"]], aes(year, TotIceDur, col = model)) + geom_line() + geom_point() 


# input_yaml_multiple(config_file, value =  "netcdf", key1 = "output",
#                     key2 = "format")
# 




