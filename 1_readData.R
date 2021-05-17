setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Load libraries for post-processing
library(lubridate)
library(gotmtools)
library(ggplot2)
library(ggpubr)

lakes = c('mendota', 'monona')

for (lake.id in lakes){
  bathy <- read.csv(paste0('numerical/',lake.id,'/3_scenarios/1_null/LakeEnsemblR_bathymetry_standard.csv'))
  colnames(bathy) <- c("depths", "areas")
  
  meteo <- read.csv(paste0('numerical/',lake.id,'/3_scenarios/1_null/LakeEnsemblR_meteo_standard.csv'))
  wind <- data.frame('datetime' = meteo$datetime, 'wnd' = meteo$Ten_Meter_Elevation_Wind_Speed_meterPerSecond) %>%
    group_by(date(datetime)) %>%
    dplyr::summarise(mean = mean(wnd)) %>%
    dplyr::rename(datetime = `date(datetime)`, wnd = mean)
  wind$datetime <- as.POSIXct(wind$datetime)
  
  ncdf_null <- paste0('numerical/',lake.id,'/3_scenarios/1_null/output/ensemble_output.nc')
  wtr_null <- load_var(ncdf = ncdf_null, var = "temp")
  density_null <- load_var(ncdf = ncdf_null, var = "dens")
  salt_null <- load_var(ncdf = ncdf_null, var = "salt")
  ice_null <- load_var(ncdf = ncdf_null, var = 'ice_height')
  
  ncdf_01 <- paste0('numerical/',lake.id,'/3_scenarios/2_constantsalt/output/ensemble_output.nc')
  wtr_01 <- load_var(ncdf = ncdf_01, var = "temp")
  density_01 <- load_var(ncdf = ncdf_01, var = "dens")
  salt_01 <- load_var(ncdf = ncdf_01, var = "salt")
  ice_01 <- load_var(ncdf = ncdf_01, var = 'ice_height')
  
  ncdf_05 <- paste0('numerical/',lake.id,'/3_scenarios/6_constantsalt+05/output/ensemble_output.nc')
  wtr_05 <- load_var(ncdf = ncdf_05, var = "temp")
  density_05 <- load_var(ncdf = ncdf_05, var = "dens")
  salt_05 <- load_var(ncdf = ncdf_05, var = "salt")
  ice_05 <- load_var(ncdf = ncdf_05, var = 'ice_height')
  
  ncdf_1 <- paste0('numerical/',lake.id,'/3_scenarios/7_constantsalt+1/output/ensemble_output.nc')
  wtr_1 <- load_var(ncdf = ncdf_1, var = "temp")
  density_1 <- load_var(ncdf = ncdf_1, var = "dens")
  salt_1 <- load_var(ncdf = ncdf_1, var = "salt")
  ice_1 <- load_var(ncdf = ncdf_1, var = 'ice_height')
  
  ncdf_15 <- paste0('numerical/',lake.id,'/3_scenarios/8_constantsalt+15/output/ensemble_output.nc')
  wtr_15 <- load_var(ncdf = ncdf_15, var = "temp")
  density_15 <- load_var(ncdf = ncdf_15, var = "dens")
  salt_15 <- load_var(ncdf = ncdf_15, var = "salt")
  ice_15 <- load_var(ncdf = ncdf_15, var = 'ice_height')
  
  ncdf_2 <- paste0('numerical/',lake.id,'/3_scenarios/3_constantsalt+2/output/ensemble_output.nc')
  wtr_2 <- load_var(ncdf = ncdf_2, var = "temp")
  density_2 <- load_var(ncdf = ncdf_2, var = "dens")
  salt_2 <- load_var(ncdf = ncdf_2, var = "salt")
  ice_2 <- load_var(ncdf = ncdf_2, var = 'ice_height')
  
  ncdf_5 <- paste0('numerical/',lake.id,'/3_scenarios/4_constantsalt+5/output/ensemble_output.nc')
  wtr_5 <- load_var(ncdf = ncdf_5, var = "temp")
  density_5 <- load_var(ncdf = ncdf_5, var = "dens")
  salt_5 <- load_var(ncdf = ncdf_5, var = "salt")
  ice_5 <- load_var(ncdf = ncdf_5, var = 'ice_height')
  
  ncdf_10 <- paste0('numerical/',lake.id,'/3_scenarios/5_constantsalt+10/output/ensemble_output.nc')
  wtr_10 <- load_var(ncdf = ncdf_10, var = "temp")
  density_10 <- load_var(ncdf = ncdf_10, var = "dens")
  salt_10 <- load_var(ncdf = ncdf_10, var = "salt")
  ice_10 <- load_var(ncdf = ncdf_10, var = 'ice_height')
  
  ncdf_25 <- paste0('numerical/',lake.id,'/3_scenarios/9_constantsalt+25/output/ensemble_output.nc')
  wtr_25 <- load_var(ncdf = ncdf_25, var = "temp")
  density_25 <- load_var(ncdf = ncdf_25, var = "dens")
  salt_25 <- load_var(ncdf = ncdf_25, var = "salt")
  ice_25 <- load_var(ncdf = ncdf_25, var = 'ice_height')
  
  ncdf_3 <- paste0('numerical/',lake.id,'/3_scenarios/10_constantsalt+3/output/ensemble_output.nc')
  wtr_3 <- load_var(ncdf = ncdf_3, var = "temp")
  density_3 <- load_var(ncdf = ncdf_3, var = "dens")
  salt_3 <- load_var(ncdf = ncdf_3, var = "salt")
  ice_3 <- load_var(ncdf = ncdf_3, var = 'ice_height')
  
  ncdf_35 <- paste0('numerical/',lake.id,'/3_scenarios/11_constantsalt+35/output/ensemble_output.nc')
  wtr_35 <- load_var(ncdf = ncdf_35, var = "temp")
  density_35 <- load_var(ncdf = ncdf_35, var = "dens")
  salt_35 <- load_var(ncdf = ncdf_35, var = "salt")
  ice_35 <- load_var(ncdf = ncdf_35, var = 'ice_height')
  
  ncdf_4 <- paste0('numerical/',lake.id,'/3_scenarios/12_constantsalt+4/output/ensemble_output.nc')
  wtr_4 <- load_var(ncdf = ncdf_4, var = "temp")
  density_4 <- load_var(ncdf = ncdf_4, var = "dens")
  salt_4 <- load_var(ncdf = ncdf_4, var = "salt")
  ice_4 <- load_var(ncdf = ncdf_4, var = 'ice_height')
  
  ncdf_45 <- paste0('numerical/',lake.id,'/3_scenarios/13_constantsalt+45/output/ensemble_output.nc')
  wtr_45 <- load_var(ncdf = ncdf_45, var = "temp")
  density_45 <- load_var(ncdf = ncdf_45, var = "dens")
  salt_45 <- load_var(ncdf = ncdf_45, var = "salt")
  ice_45 <- load_var(ncdf = ncdf_45, var = 'ice_height')
  
  if (match(lake.id, lakes) == 1){
    df <- data.frame('datetime' = wtr_null$GLM$datetime,
                     'dens_null' = apply(cbind(density_null$GLM$wtr_22 - density_null$GLM$wtr_2,
                                               density_null$GOTM$wtr_22 - density_null$GOTM$wtr_2,
                                               density_null$Simstrat$wtr_22 - density_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_01' = apply(cbind(density_01$GLM$wtr_22 - density_01$GLM$wtr_2,
                                             density_01$GOTM$wtr_22 - density_01$GOTM$wtr_2,
                                             density_01$Simstrat$wtr_22 - density_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_05' = apply(cbind(density_05$GLM$wtr_22 - density_05$GLM$wtr_2,
                                             density_05$GOTM$wtr_22 - density_05$GOTM$wtr_2,
                                             density_05$Simstrat$wtr_22 - density_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_1' = apply(cbind(density_1$GLM$wtr_22 - density_1$GLM$wtr_2,
                                            density_1$GOTM$wtr_22 - density_1$GOTM$wtr_2,
                                            density_1$Simstrat$wtr_22 - density_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_15' = apply(cbind(density_15$GLM$wtr_22 - density_15$GLM$wtr_2,
                                             density_15$GOTM$wtr_22 - density_15$GOTM$wtr_2,
                                             density_15$Simstrat$wtr_22 - density_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_2' = apply(cbind(density_2$GLM$wtr_22 - density_2$GLM$wtr_2,
                                            density_2$GOTM$wtr_22 - density_2$GOTM$wtr_2,
                                            density_2$Simstrat$wtr_22 - density_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_25' = apply(cbind(density_25$GLM$wtr_22 - density_25$GLM$wtr_2,
                                             density_25$GOTM$wtr_22 - density_25$GOTM$wtr_2,
                                             density_25$Simstrat$wtr_22 - density_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_3' = apply(cbind(density_3$GLM$wtr_22 - density_3$GLM$wtr_2,
                                            density_3$GOTM$wtr_22 - density_3$GOTM$wtr_2,
                                            density_3$Simstrat$wtr_22 - density_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_35' = apply(cbind(density_35$GLM$wtr_22 - density_35$GLM$wtr_2,
                                             density_35$GOTM$wtr_22 - density_35$GOTM$wtr_2,
                                             density_35$Simstrat$wtr_22 - density_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_4' = apply(cbind(density_4$GLM$wtr_22 - density_4$GLM$wtr_2,
                                            density_4$GOTM$wtr_22 - density_4$GOTM$wtr_2,
                                            density_4$Simstrat$wtr_22 - density_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_45' = apply(cbind(density_45$GLM$wtr_22 - density_45$GLM$wtr_2,
                                             density_45$GOTM$wtr_22 - density_45$GOTM$wtr_2,
                                             density_45$Simstrat$wtr_22 - density_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_5' = apply(cbind(density_5$GLM$wtr_22 - density_5$GLM$wtr_2,
                                            density_5$GOTM$wtr_22 - density_5$GOTM$wtr_2,
                                            density_5$Simstrat$wtr_22 - density_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_10' = apply(cbind(density_10$GLM$wtr_22 - density_10$GLM$wtr_2,
                                             density_10$GOTM$wtr_22 - density_10$GOTM$wtr_2,
                                             density_10$Simstrat$wtr_22 - density_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df$id = lake.id
    
    df_wtr <- data.frame('datetime' = wtr_null$GLM$datetime,
                         'temp_null' = apply(cbind(wtr_null$GLM$wtr_22 - wtr_null$GLM$wtr_2,
                                                   wtr_null$GOTM$wtr_22 - wtr_null$GOTM$wtr_2,
                                                   wtr_null$Simstrat$wtr_22 - wtr_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_01' = apply(cbind(wtr_01$GLM$wtr_22 - wtr_01$GLM$wtr_2,
                                                 wtr_01$GOTM$wtr_22 - wtr_01$GOTM$wtr_2,
                                                 wtr_01$Simstrat$wtr_22 - wtr_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_05' = apply(cbind(wtr_05$GLM$wtr_22 - wtr_05$GLM$wtr_2,
                                                 wtr_05$GOTM$wtr_22 - wtr_05$GOTM$wtr_2,
                                                 wtr_05$Simstrat$wtr_22 - wtr_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_1' = apply(cbind(wtr_1$GLM$wtr_22 - wtr_1$GLM$wtr_2,
                                                wtr_1$GOTM$wtr_22 - wtr_1$GOTM$wtr_2,
                                                wtr_1$Simstrat$wtr_22 - wtr_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_15' = apply(cbind(wtr_15$GLM$wtr_22 - wtr_15$GLM$wtr_2,
                                                 wtr_15$GOTM$wtr_22 - wtr_15$GOTM$wtr_2,
                                                 wtr_15$Simstrat$wtr_22 - wtr_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_2' = apply(cbind(wtr_2$GLM$wtr_22 - wtr_2$GLM$wtr_2,
                                                wtr_2$GOTM$wtr_22 - wtr_2$GOTM$wtr_2,
                                                wtr_2$Simstrat$wtr_22 - wtr_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_25' = apply(cbind(wtr_25$GLM$wtr_22 - wtr_25$GLM$wtr_2,
                                                 wtr_25$GOTM$wtr_22 - wtr_25$GOTM$wtr_2,
                                                 wtr_25$Simstrat$wtr_22 - wtr_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_3' = apply(cbind(wtr_3$GLM$wtr_22 - wtr_3$GLM$wtr_2,
                                                wtr_3$GOTM$wtr_22 - wtr_3$GOTM$wtr_2,
                                                wtr_3$Simstrat$wtr_22 - wtr_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_35' = apply(cbind(wtr_35$GLM$wtr_22 - wtr_35$GLM$wtr_2,
                                                 wtr_35$GOTM$wtr_22 - wtr_35$GOTM$wtr_2,
                                                 wtr_35$Simstrat$wtr_22 - wtr_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_4' = apply(cbind(wtr_4$GLM$wtr_22 - wtr_4$GLM$wtr_2,
                                                wtr_4$GOTM$wtr_22 - wtr_4$GOTM$wtr_2,
                                                wtr_4$Simstrat$wtr_22 - wtr_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_45' = apply(cbind(wtr_45$GLM$wtr_22 - wtr_45$GLM$wtr_2,
                                                 wtr_45$GOTM$wtr_22 - wtr_45$GOTM$wtr_2,
                                                 wtr_45$Simstrat$wtr_22 - wtr_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_5' = apply(cbind(wtr_5$GLM$wtr_22 - wtr_5$GLM$wtr_2,
                                                wtr_5$GOTM$wtr_22 - wtr_5$GOTM$wtr_2,
                                                wtr_5$Simstrat$wtr_22 - wtr_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_10' = apply(cbind(wtr_10$GLM$wtr_22 - wtr_10$GLM$wtr_2,
                                                 wtr_10$GOTM$wtr_22 - wtr_10$GOTM$wtr_2,
                                                 wtr_10$Simstrat$wtr_22 - wtr_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df_wtr$id = lake.id
    
    df_ice<- data.frame('datetime' = ice_null$GLM$time,
                        'ice_null' = apply(cbind(ice_null$GLM$ice_height,ice_null$GOTM$ice_height,ice_null$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_01' = apply(cbind(ice_01$GLM$ice_height,ice_01$GOTM$ice_height,ice_01$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_05' = apply(cbind(ice_05$GLM$ice_height,ice_05$GOTM$ice_height,ice_05$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_1' = apply(cbind(ice_1$GLM$ice_height,ice_1$GOTM$ice_height,ice_1$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_15' = apply(cbind(ice_15$GLM$ice_height,ice_15$GOTM$ice_height,ice_15$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_2' = apply(cbind(ice_2$GLM$ice_height,ice_2$GOTM$ice_height,ice_2$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_25' = apply(cbind(ice_25$GLM$ice_height,ice_25$GOTM$ice_height,ice_25$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_3' = apply(cbind(ice_3$GLM$ice_height,ice_3$GOTM$ice_height,ice_3$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_35' = apply(cbind(ice_35$GLM$ice_height,ice_35$GOTM$ice_height,ice_35$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_4' = apply(cbind(ice_4$GLM$ice_height,ice_4$GOTM$ice_height,ice_4$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_45' = apply(cbind(ice_45$GLM$ice_height,ice_45$GOTM$ice_height,ice_45$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_5' = apply(cbind(ice_5$GLM$ice_height,ice_5$GOTM$ice_height,ice_5$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                        'ice_10' = apply(cbind(ice_10$GLM$ice_height,ice_10$GOTM$ice_height,ice_10$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE))
    )
    df_ice$id = lake.id
    
    df_ssi <- data.frame('datetime' = wtr_null$GLM$datetime,
                         'ssi_null' = apply(cbind(
                           lapply(wtr_null, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_null, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_null, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_01' = apply(cbind(
                           lapply(wtr_01, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_01, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_01, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_05' = apply(cbind(
                           lapply(wtr_05, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_05, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_05, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_1' =apply(cbind(
                           lapply(wtr_1, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_1, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_1, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_15' = apply(cbind(
                           lapply(wtr_15, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_15, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_15, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_2' = apply(cbind(
                           lapply(wtr_2, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_2, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_2, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_25' = apply(cbind(
                           lapply(wtr_25, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_25, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_25, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_3' = apply(cbind(
                           lapply(wtr_3, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_3, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_3, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_35' = apply(cbind(
                           lapply(wtr_35, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_35, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_35, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_4' = apply(cbind(
                           lapply(wtr_4, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_4, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_4, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_45' = apply(cbind(
                           lapply(wtr_45, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_45, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_45, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_5' = apply(cbind(
                           lapply(wtr_5, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_5, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_5, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE)),
                         'ssi_10' = apply(cbind(
                           lapply(wtr_10, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GLM$schmidt.stability, 
                           lapply(wtr_10, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$GOTM$schmidt.stability,
                           lapply(wtr_10, function(x) {
                             ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                           })$Simstrat$schmidt.stability), 
                           1, function(x) mean(x, na.rm = TRUE))
    )
    df_ssi$id = lake.id
    
    df_ln <- data.frame('datetime' = wtr_null$GLM$datetime,
                        'ln_null' = apply(cbind(
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_01' = apply(cbind(
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_05' = apply(cbind(
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_1' = apply(cbind(
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_15' = apply(cbind(
                          lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_2' = apply(cbind(
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_25' = apply(cbind(
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_3' = apply(cbind(
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_n35' = apply(cbind(
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_4' = apply(cbind(
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_45' = apply(cbind(
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_5' = apply(cbind(
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_10' = apply(cbind(
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE))
    )
    df_ln$id = lake.id
    

    
  } else {
    df2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                      'dens_null' = apply(cbind(density_null$GLM$wtr_22 - density_null$GLM$wtr_2,
                                                density_null$GOTM$wtr_22 - density_null$GOTM$wtr_2,
                                                density_null$Simstrat$wtr_22 - density_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_01' = apply(cbind(density_01$GLM$wtr_22 - density_01$GLM$wtr_2,
                                              density_01$GOTM$wtr_22 - density_01$GOTM$wtr_2,
                                              density_01$Simstrat$wtr_22 - density_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_05' = apply(cbind(density_05$GLM$wtr_22 - density_05$GLM$wtr_2,
                                              density_05$GOTM$wtr_22 - density_05$GOTM$wtr_2,
                                              density_05$Simstrat$wtr_22 - density_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_1' = apply(cbind(density_1$GLM$wtr_22 - density_1$GLM$wtr_2,
                                             density_1$GOTM$wtr_22 - density_1$GOTM$wtr_2,
                                             density_1$Simstrat$wtr_22 - density_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_15' = apply(cbind(density_15$GLM$wtr_22 - density_15$GLM$wtr_2,
                                              density_15$GOTM$wtr_22 - density_15$GOTM$wtr_2,
                                              density_15$Simstrat$wtr_22 - density_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_2' = apply(cbind(density_2$GLM$wtr_22 - density_2$GLM$wtr_2,
                                             density_2$GOTM$wtr_22 - density_2$GOTM$wtr_2,
                                             density_2$Simstrat$wtr_22 - density_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_25' = apply(cbind(density_25$GLM$wtr_22 - density_25$GLM$wtr_2,
                                              density_25$GOTM$wtr_22 - density_25$GOTM$wtr_2,
                                              density_25$Simstrat$wtr_22 - density_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_3' = apply(cbind(density_3$GLM$wtr_22 - density_3$GLM$wtr_2,
                                             density_3$GOTM$wtr_22 - density_3$GOTM$wtr_2,
                                             density_3$Simstrat$wtr_22 - density_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_35' = apply(cbind(density_35$GLM$wtr_22 - density_35$GLM$wtr_2,
                                              density_35$GOTM$wtr_22 - density_35$GOTM$wtr_2,
                                              density_35$Simstrat$wtr_22 - density_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_4' = apply(cbind(density_4$GLM$wtr_22 - density_4$GLM$wtr_2,
                                             density_4$GOTM$wtr_22 - density_4$GOTM$wtr_2,
                                             density_4$Simstrat$wtr_22 - density_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_45' = apply(cbind(density_45$GLM$wtr_22 - density_45$GLM$wtr_2,
                                              density_45$GOTM$wtr_22 - density_45$GOTM$wtr_2,
                                              density_45$Simstrat$wtr_22 - density_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_5' = apply(cbind(density_5$GLM$wtr_22 - density_5$GLM$wtr_2,
                                             density_5$GOTM$wtr_22 - density_5$GOTM$wtr_2,
                                             density_5$Simstrat$wtr_22 - density_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_10' = apply(cbind(density_10$GLM$wtr_22 - density_10$GLM$wtr_2,
                                              density_10$GOTM$wtr_22 - density_10$GOTM$wtr_2,
                                              density_10$Simstrat$wtr_22 - density_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df2$id = lake.id
    
    df_wtr2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                          'temp_null' = apply(cbind(wtr_null$GLM$wtr_22 - wtr_null$GLM$wtr_2,
                                                    wtr_null$GOTM$wtr_22 - wtr_null$GOTM$wtr_2,
                                                    wtr_null$Simstrat$wtr_22 - wtr_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_01' = apply(cbind(wtr_01$GLM$wtr_22 - wtr_01$GLM$wtr_2,
                                                  wtr_01$GOTM$wtr_22 - wtr_01$GOTM$wtr_2,
                                                  wtr_01$Simstrat$wtr_22 - wtr_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_05' = apply(cbind(wtr_05$GLM$wtr_22 - wtr_05$GLM$wtr_2,
                                                  wtr_05$GOTM$wtr_22 - wtr_05$GOTM$wtr_2,
                                                  wtr_05$Simstrat$wtr_22 - wtr_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_1' = apply(cbind(wtr_1$GLM$wtr_22 - wtr_1$GLM$wtr_2,
                                                 wtr_1$GOTM$wtr_22 - wtr_1$GOTM$wtr_2,
                                                 wtr_1$Simstrat$wtr_22 - wtr_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_15' = apply(cbind(wtr_15$GLM$wtr_22 - wtr_15$GLM$wtr_2,
                                                  wtr_15$GOTM$wtr_22 - wtr_15$GOTM$wtr_2,
                                                  wtr_15$Simstrat$wtr_22 - wtr_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_2' = apply(cbind(wtr_2$GLM$wtr_22 - wtr_2$GLM$wtr_2,
                                                 wtr_2$GOTM$wtr_22 - wtr_2$GOTM$wtr_2,
                                                 wtr_2$Simstrat$wtr_22 - wtr_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_25' = apply(cbind(wtr_25$GLM$wtr_22 - wtr_25$GLM$wtr_2,
                                                  wtr_25$GOTM$wtr_22 - wtr_25$GOTM$wtr_2,
                                                  wtr_25$Simstrat$wtr_22 - wtr_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_3' = apply(cbind(wtr_3$GLM$wtr_22 - wtr_3$GLM$wtr_2,
                                                 wtr_3$GOTM$wtr_22 - wtr_3$GOTM$wtr_2,
                                                 wtr_3$Simstrat$wtr_22 - wtr_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_35' = apply(cbind(wtr_35$GLM$wtr_22 - wtr_35$GLM$wtr_2,
                                                  wtr_35$GOTM$wtr_22 - wtr_35$GOTM$wtr_2,
                                                  wtr_35$Simstrat$wtr_22 - wtr_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_4' = apply(cbind(wtr_4$GLM$wtr_22 - wtr_4$GLM$wtr_2,
                                                 wtr_4$GOTM$wtr_22 - wtr_4$GOTM$wtr_2,
                                                 wtr_4$Simstrat$wtr_22 - wtr_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_45' = apply(cbind(wtr_45$GLM$wtr_22 - wtr_45$GLM$wtr_2,
                                                  wtr_45$GOTM$wtr_22 - wtr_45$GOTM$wtr_2,
                                                  wtr_45$Simstrat$wtr_22 - wtr_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_5' = apply(cbind(wtr_5$GLM$wtr_22 - wtr_5$GLM$wtr_2,
                                                 wtr_5$GOTM$wtr_22 - wtr_5$GOTM$wtr_2,
                                                 wtr_5$Simstrat$wtr_22 - wtr_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_10' = apply(cbind(wtr_10$GLM$wtr_22 - wtr_10$GLM$wtr_2,
                                                  wtr_10$GOTM$wtr_22 - wtr_10$GOTM$wtr_2,
                                                  wtr_10$Simstrat$wtr_22 - wtr_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df_wtr2$id = lake.id
    
    df_ice2<- data.frame('datetime' =  ice_null$GLM$time,
                         'ice_null' = apply(cbind(ice_null$GLM$ice_height,ice_null$GOTM$ice_height,ice_null$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_01' = apply(cbind(ice_01$GLM$ice_height,ice_01$GOTM$ice_height,ice_01$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_05' = apply(cbind(ice_05$GLM$ice_height,ice_05$GOTM$ice_height,ice_05$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_1' = apply(cbind(ice_1$GLM$ice_height,ice_1$GOTM$ice_height,ice_1$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_15' = apply(cbind(ice_15$GLM$ice_height,ice_15$GOTM$ice_height,ice_15$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_2' = apply(cbind(ice_2$GLM$ice_height,ice_2$GOTM$ice_height,ice_2$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_25' = apply(cbind(ice_25$GLM$ice_height,ice_25$GOTM$ice_height,ice_25$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_3' = apply(cbind(ice_3$GLM$ice_height,ice_3$GOTM$ice_height,ice_3$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_35' = apply(cbind(ice_35$GLM$ice_height,ice_35$GOTM$ice_height,ice_35$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_4' = apply(cbind(ice_4$GLM$ice_height,ice_4$GOTM$ice_height,ice_4$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_45' = apply(cbind(ice_45$GLM$ice_height,ice_45$GOTM$ice_height,ice_45$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_5' = apply(cbind(ice_5$GLM$ice_height,ice_5$GOTM$ice_height,ice_5$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE)),
                         'ice_10' = apply(cbind(ice_10$GLM$ice_height,ice_10$GOTM$ice_height,ice_10$Simstrat$ice_height), 1, function(x) mean(x, na.rm = TRUE))
    )
    df_ice2$id = lake.id
    df_ssi2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                          'ssi_null' = apply(cbind(
                            lapply(wtr_null, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_null, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_null, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_01' = apply(cbind(
                            lapply(wtr_01, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_01, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_01, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_05' = apply(cbind(
                            lapply(wtr_05, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_05, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_05, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_1' =apply(cbind(
                            lapply(wtr_1, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_1, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_1, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_15' = apply(cbind(
                            lapply(wtr_15, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_15, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_15, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_2' = apply(cbind(
                            lapply(wtr_2, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_2, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_2, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_25' = apply(cbind(
                            lapply(wtr_25, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_25, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_25, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_3' = apply(cbind(
                            lapply(wtr_3, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_3, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_3, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_35' = apply(cbind(
                            lapply(wtr_35, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_35, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_35, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_4' = apply(cbind(
                            lapply(wtr_4, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_4, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_4, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_45' = apply(cbind(
                            lapply(wtr_45, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_45, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_45, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_5' = apply(cbind(
                            lapply(wtr_5, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_5, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_5, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE)),
                          'ssi_10' = apply(cbind(
                            lapply(wtr_10, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GLM$schmidt.stability, 
                            lapply(wtr_10, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$GOTM$schmidt.stability,
                            lapply(wtr_10, function(x) {
                              ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                            })$Simstrat$schmidt.stability), 
                            1, function(x) mean(x, na.rm = TRUE))
    )
    df_ssi2$id = lake.id
    df_ln2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                        'ln_null' = apply(cbind(
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_01' = apply(cbind(
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_05' = apply(cbind(
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_1' = apply(cbind(
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_15' = apply(cbind(
                          lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_2' = apply(cbind(
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_25' = apply(cbind(
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_3' = apply(cbind(
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_n35' = apply(cbind(
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_4' = apply(cbind(
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_45' = apply(cbind(
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_5' = apply(cbind(
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE)),
                        'ln_10' = apply(cbind(
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number, 
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                          lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number),
                          1, function(x) mean(x, na.rm = TRUE))
    )
    df_ln2$id = lake.id
    
    df = rbind(df, df2)
    df_wtr = rbind(df_wtr, df_wtr2)
    df_ssi = rbind(df_ssi, df_ssi2)
    df_ice <- rbind(df_ice, df_ice2)
    df_ln <- rbind(df_ln, df_ln2)
  }
}

write.csv(x = df, file = 'output/density.csv', quote = F, row.names = F)
write.csv(x = df_wtr, file = 'output/wtemp.csv', quote = F, row.names = F)
write.csv(x = df_ssi, file = 'output/ssi.csv', quote = F, row.names = F)
write.csv(x = df_ice, file = 'output/ice.csv', quote = F, row.names = F)
write.csv(x = df_ln, file = 'output/lakenumber.csv', quote = F, row.names = F)

