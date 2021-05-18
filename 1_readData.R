rm(list = ls())

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
  
  print(paste0('loaded all variables for ',lake.id))
  
  if (match(lake.id, lakes) == 1){
    df <- data.frame('datetime' = wtr_null$GLM$datetime,
                     'dens_null' = apply(cbind(density_null$GLM$wtr_20 - density_null$GLM$wtr_2,
                                               density_null$GOTM$wtr_20 - density_null$GOTM$wtr_2,
                                               density_null$Simstrat$wtr_20 - density_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_01' = apply(cbind(density_01$GLM$wtr_20 - density_01$GLM$wtr_2,
                                             density_01$GOTM$wtr_20 - density_01$GOTM$wtr_2,
                                             density_01$Simstrat$wtr_20 - density_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_05' = apply(cbind(density_05$GLM$wtr_20 - density_05$GLM$wtr_2,
                                             density_05$GOTM$wtr_20 - density_05$GOTM$wtr_2,
                                             density_05$Simstrat$wtr_20 - density_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_1' = apply(cbind(density_1$GLM$wtr_20 - density_1$GLM$wtr_2,
                                            density_1$GOTM$wtr_20 - density_1$GOTM$wtr_2,
                                            density_1$Simstrat$wtr_20 - density_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_15' = apply(cbind(density_15$GLM$wtr_20 - density_15$GLM$wtr_2,
                                             density_15$GOTM$wtr_20 - density_15$GOTM$wtr_2,
                                             density_15$Simstrat$wtr_20 - density_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_2' = apply(cbind(density_2$GLM$wtr_20 - density_2$GLM$wtr_2,
                                            density_2$GOTM$wtr_20 - density_2$GOTM$wtr_2,
                                            density_2$Simstrat$wtr_20 - density_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_25' = apply(cbind(density_25$GLM$wtr_20 - density_25$GLM$wtr_2,
                                             density_25$GOTM$wtr_20 - density_25$GOTM$wtr_2,
                                             density_25$Simstrat$wtr_20 - density_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_3' = apply(cbind(density_3$GLM$wtr_20 - density_3$GLM$wtr_2,
                                            density_3$GOTM$wtr_20 - density_3$GOTM$wtr_2,
                                            density_3$Simstrat$wtr_20 - density_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_35' = apply(cbind(density_35$GLM$wtr_20 - density_35$GLM$wtr_2,
                                             density_35$GOTM$wtr_20 - density_35$GOTM$wtr_2,
                                             density_35$Simstrat$wtr_20 - density_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_4' = apply(cbind(density_4$GLM$wtr_20 - density_4$GLM$wtr_2,
                                            density_4$GOTM$wtr_20 - density_4$GOTM$wtr_2,
                                            density_4$Simstrat$wtr_20 - density_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_45' = apply(cbind(density_45$GLM$wtr_20 - density_45$GLM$wtr_2,
                                             density_45$GOTM$wtr_20 - density_45$GOTM$wtr_2,
                                             density_45$Simstrat$wtr_20 - density_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_5' = apply(cbind(density_5$GLM$wtr_20 - density_5$GLM$wtr_2,
                                            density_5$GOTM$wtr_20 - density_5$GOTM$wtr_2,
                                            density_5$Simstrat$wtr_20 - density_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                     'dens_10' = apply(cbind(density_10$GLM$wtr_20 - density_10$GLM$wtr_2,
                                             density_10$GOTM$wtr_20 - density_10$GOTM$wtr_2,
                                             density_10$Simstrat$wtr_20 - density_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df$id = lake.id
    
    df_wtr <- data.frame('datetime' = wtr_null$GLM$datetime,
                         'temp_null' = apply(cbind(wtr_null$GLM$wtr_20 - wtr_null$GLM$wtr_2,
                                                   wtr_null$GOTM$wtr_20 - wtr_null$GOTM$wtr_2,
                                                   wtr_null$Simstrat$wtr_20 - wtr_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_01' = apply(cbind(wtr_01$GLM$wtr_20 - wtr_01$GLM$wtr_2,
                                                 wtr_01$GOTM$wtr_20 - wtr_01$GOTM$wtr_2,
                                                 wtr_01$Simstrat$wtr_20 - wtr_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_05' = apply(cbind(wtr_05$GLM$wtr_20 - wtr_05$GLM$wtr_2,
                                                 wtr_05$GOTM$wtr_20 - wtr_05$GOTM$wtr_2,
                                                 wtr_05$Simstrat$wtr_20 - wtr_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_1' = apply(cbind(wtr_1$GLM$wtr_20 - wtr_1$GLM$wtr_2,
                                                wtr_1$GOTM$wtr_20 - wtr_1$GOTM$wtr_2,
                                                wtr_1$Simstrat$wtr_20 - wtr_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_15' = apply(cbind(wtr_15$GLM$wtr_20 - wtr_15$GLM$wtr_2,
                                                 wtr_15$GOTM$wtr_20 - wtr_15$GOTM$wtr_2,
                                                 wtr_15$Simstrat$wtr_20 - wtr_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_2' = apply(cbind(wtr_2$GLM$wtr_20 - wtr_2$GLM$wtr_2,
                                                wtr_2$GOTM$wtr_20 - wtr_2$GOTM$wtr_2,
                                                wtr_2$Simstrat$wtr_20 - wtr_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_25' = apply(cbind(wtr_25$GLM$wtr_20 - wtr_25$GLM$wtr_2,
                                                 wtr_25$GOTM$wtr_20 - wtr_25$GOTM$wtr_2,
                                                 wtr_25$Simstrat$wtr_20 - wtr_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_3' = apply(cbind(wtr_3$GLM$wtr_20 - wtr_3$GLM$wtr_2,
                                                wtr_3$GOTM$wtr_20 - wtr_3$GOTM$wtr_2,
                                                wtr_3$Simstrat$wtr_20 - wtr_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_35' = apply(cbind(wtr_35$GLM$wtr_20 - wtr_35$GLM$wtr_2,
                                                 wtr_35$GOTM$wtr_20 - wtr_35$GOTM$wtr_2,
                                                 wtr_35$Simstrat$wtr_20 - wtr_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_4' = apply(cbind(wtr_4$GLM$wtr_20 - wtr_4$GLM$wtr_2,
                                                wtr_4$GOTM$wtr_20 - wtr_4$GOTM$wtr_2,
                                                wtr_4$Simstrat$wtr_20 - wtr_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_45' = apply(cbind(wtr_45$GLM$wtr_20 - wtr_45$GLM$wtr_2,
                                                 wtr_45$GOTM$wtr_20 - wtr_45$GOTM$wtr_2,
                                                 wtr_45$Simstrat$wtr_20 - wtr_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_5' = apply(cbind(wtr_5$GLM$wtr_20 - wtr_5$GLM$wtr_2,
                                                wtr_5$GOTM$wtr_20 - wtr_5$GOTM$wtr_2,
                                                wtr_5$Simstrat$wtr_20 - wtr_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                         'temp_10' = apply(cbind(wtr_10$GLM$wtr_20 - wtr_10$GLM$wtr_2,
                                                 wtr_10$GOTM$wtr_20 - wtr_10$GOTM$wtr_2,
                                                 wtr_10$Simstrat$wtr_20 - wtr_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
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
                        'ln_35' = apply(cbind(
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
    
    print(paste0('calculated all scenario variables for ',lake.id))
    
    df_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                         'dens_null_GLM' = density_null$GLM$wtr_20 - density_null$GLM$wtr_2,
                         
                         'dens_01_GLM' = density_01$GLM$wtr_20 - density_01$GLM$wtr_2,
                         
                         'dens_05_GLM' = density_05$GLM$wtr_20 - density_05$GLM$wtr_2,
                         
                         'dens_1_GLM' = density_1$GLM$wtr_20 - density_1$GLM$wtr_2,
                         
                         'dens_15_GLM' = density_15$GLM$wtr_20 - density_15$GLM$wtr_2,
                         
                         'dens_2_GLM' = density_2$GLM$wtr_20 - density_2$GLM$wtr_2,
                         
                         'dens_25_GLM' = density_25$GLM$wtr_20 - density_25$GLM$wtr_2,
                         
                         'dens_3_GLM' = density_3$GLM$wtr_20 - density_3$GLM$wtr_2,
                         
                         'dens_35_GLM' = density_35$GLM$wtr_20 - density_35$GLM$wtr_2,
                         
                         'dens_4_GLM' = density_4$GLM$wtr_20 - density_4$GLM$wtr_2,
                         
                         'dens_45_GLM' = density_45$GLM$wtr_20 - density_45$GLM$wtr_2,
                         
                         'dens_5_GLM' = density_5$GLM$wtr_20 - density_5$GLM$wtr_2,
                         
                         'dens_10_GLM' = density_10$GLM$wtr_20 - density_10$GLM$wtr_2
                         
    )
    df_GLM$id = lake.id
    
    df_wtr_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                             'temp_null_GLM' = wtr_null$GLM$wtr_20 - wtr_null$GLM$wtr_2,
                             
                             'temp_01_GLM' = wtr_01$GLM$wtr_20 - wtr_01$GLM$wtr_2,
                             
                             'temp_05_GLM' = wtr_05$GLM$wtr_20 - wtr_05$GLM$wtr_2,
                             
                             'temp_1_GLM' = wtr_1$GLM$wtr_20 - wtr_1$GLM$wtr_2,
                             
                             'temp_15_GLM' = wtr_15$GLM$wtr_20 - wtr_15$GLM$wtr_2,
                             
                             'temp_2_GLM' = wtr_2$GLM$wtr_20 - wtr_2$GLM$wtr_2,
                             
                             'temp_25_GLM' = wtr_25$GLM$wtr_20 - wtr_25$GLM$wtr_2,
                             
                             'temp_3_GLM' = wtr_3$GLM$wtr_20 - wtr_3$GLM$wtr_2,
                             
                             'temp_35_GLM' = wtr_35$GLM$wtr_20 - wtr_35$GLM$wtr_2,
                             
                             'temp_4_GLM' = wtr_4$GLM$wtr_20 - wtr_4$GLM$wtr_2,
                             
                             'temp_45_GLM' = wtr_45$GLM$wtr_20 - wtr_45$GLM$wtr_2,
                             
                             'temp_5_GLM' = wtr_5$GLM$wtr_20 - wtr_5$GLM$wtr_2,
                             
                             'temp_10_GLM' = wtr_10$GLM$wtr_20 - wtr_10$GLM$wtr_2
                             
    )
    df_wtr_GLM$id = lake.id
    
    df_ice_GLM<- data.frame('datetime' = ice_null$GLM$time,
                            'ice_null_GLM' = ice_null$GLM$ice_height,
                            'ice_01_GLM' = ice_01$GLM$ice_height,
                            'ice_05_GLM' = ice_05$GLM$ice_height,
                            'ice_1_GLM' = ice_1$GLM$ice_height,
                            'ice_15_GLM' = ice_15$GLM$ice_height,
                            'ice_2_GLM' = ice_2$GLM$ice_height,
                            'ice_25_GLM' = ice_25$GLM$ice_height,
                            'ice_3_GLM' = ice_3$GLM$ice_height,
                            'ice_35_GLM' = ice_35$GLM$ice_height,
                            'ice_4_GLM' = ice_4$GLM$ice_height,
                            'ice_45_GLM' = ice_45$GLM$ice_height,
                            'ice_5_GLM' = ice_5$GLM$ice_height,
                            'ice_10_GLM' = ice_10$GLM$ice_height
    )
    df_ice_GLM$id = lake.id
    
    df_ssi_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                             'ssi_null_GLM' = 
                               lapply(wtr_null, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability,
                             'ssi_01_GLM' = 
                               lapply(wtr_01, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_05_GLM' = 
                               lapply(wtr_05, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_1_GLM' =
                               lapply(wtr_1, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_15_GLM' = 
                               lapply(wtr_15, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_2_GLM' = 
                               lapply(wtr_2, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_25_GLM' = 
                               lapply(wtr_25, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_3_GLM' = 
                               lapply(wtr_3, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability, 
                             'ssi_35_GLM' = 
                               lapply(wtr_35, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability,
                             'ssi_4_GLM' = 
                               lapply(wtr_4, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability,
                             'ssi_45_GLM' = 
                               lapply(wtr_45, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability,
                             'ssi_5_GLM' = 
                               lapply(wtr_5, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability,
                             'ssi_10_GLM' = 
                               lapply(wtr_10, function(x) {
                                 ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                               })$GLM$schmidt.stability
    )
    df_ssi_GLM$id = lake.id
    
    for (i in 2:ncol(wtr_null$GLM)){
      if(any(is.na(wtr_null$GLM[-c(1),i]))){
        dummy_null = wtr_null
        dummy_null$GLM = wtr_null$GLM[,1:(i-1)]
        
        dummy_01 = wtr_01
        dummy_01$GLM = wtr_01$GLM[,1:(i-1)]
        
        dummy_05 = wtr_05
        dummy_05$GLM = wtr_05$GLM[,1:(i-1)]
        
        dummy_1 = wtr_1
        dummy_1$GLM = wtr_1$GLM[,1:(i-1)]
        
        dummy_15 = wtr_15
        dummy_15$GLM = wtr_15$GLM[,1:(i-1)]
        
        dummy_2 = wtr_2
        dummy_2$GLM = wtr_2$GLM[,1:(i-1)]
        
        dummy_25 = wtr_25
        dummy_25$GLM = wtr_25$GLM[,1:(i-1)]
        
        dummy_3 = wtr_3
        dummy_3$GLM = wtr_3$GLM[,1:(i-1)]
        
        dummy_35 = wtr_35
        dummy_35$GLM = wtr_35$GLM[,1:(i-1)]
        
        dummy_4 = wtr_4
        dummy_4$GLM = wtr_4$GLM[,1:(i-1)]
        
        dummy_45 = wtr_45
        dummy_45$GLM = wtr_45$GLM[,1:(i-1)]
        
        dummy_5 = wtr_5
        dummy_5$GLM = wtr_5$GLM[,1:(i-1)]
        
        dummy_10 = wtr_10
        dummy_10$GLM = wtr_10$GLM[,1:(i-1)]
        break
      }
    }
    df_ln_GLM <- data.frame('datetime' = dummy_null$GLM$datetime,
                            'ln_null_GLM' = 
                              lapply(dummy_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_01_GLM' = 
                              lapply(dummy_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_05_GLM' = 
                              lapply(dummy_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_1_GLM' = 
                              lapply(dummy_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_15_GLM' = 
                              lapply(dummy_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_2_GLM' = 
                              lapply(dummy_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_25_GLM' = 
                              lapply(dummy_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_3_GLM' = 
                              lapply(dummy_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_35_GLM' = 
                              lapply(dummy_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_4_GLM' = 
                              lapply(dummy_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_45_GLM' = 
                              lapply(dummy_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_5_GLM' = 
                              lapply(dummy_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                            'ln_10_GLM' = 
                              lapply(dummy_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number
    )
    df_ln_GLM$id = lake.id
    
    print(paste0('calculated all GLM variables for ',lake.id))
    
    df_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                          'dens_null_GOTM' = density_null$GOTM$wtr_20 - density_null$GOTM$wtr_2,
                          
                          'dens_01_GOTM' = density_01$GOTM$wtr_20 - density_01$GOTM$wtr_2,
                          
                          'dens_05_GOTM' = density_05$GOTM$wtr_20 - density_05$GOTM$wtr_2,
                          
                          'dens_1_GOTM' = density_1$GOTM$wtr_20 - density_1$GOTM$wtr_2,
                          
                          'dens_15_GOTM' = density_15$GOTM$wtr_20 - density_15$GOTM$wtr_2,
                          
                          'dens_2_GOTM' = density_2$GOTM$wtr_20 - density_2$GOTM$wtr_2,
                          
                          'dens_25_GOTM' = density_25$GOTM$wtr_20 - density_25$GOTM$wtr_2,
                          
                          'dens_3_GOTM' = density_3$GOTM$wtr_20 - density_3$GOTM$wtr_2,
                          
                          'dens_35_GOTM' = density_35$GOTM$wtr_20 - density_35$GOTM$wtr_2,
                          
                          'dens_4_GOTM' = density_4$GOTM$wtr_20 - density_4$GOTM$wtr_2,
                          
                          'dens_45_GOTM' = density_45$GOTM$wtr_20 - density_45$GOTM$wtr_2,
                          
                          'dens_5_GOTM' = density_5$GOTM$wtr_20 - density_5$GOTM$wtr_2,
                          
                          'dens_10_GOTM' = density_10$GOTM$wtr_20 - density_10$GOTM$wtr_2
                          
    )
    df_GOTM$id = lake.id
    
    df_wtr_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                              'temp_null_GOTM' = wtr_null$GOTM$wtr_20 - wtr_null$GOTM$wtr_2,
                              
                              'temp_01_GOTM' = wtr_01$GOTM$wtr_20 - wtr_01$GOTM$wtr_2,
                              
                              'temp_05_GOTM' = wtr_05$GOTM$wtr_20 - wtr_05$GOTM$wtr_2,
                              
                              'temp_1_GOTM' = wtr_1$GOTM$wtr_20 - wtr_1$GOTM$wtr_2,
                              
                              'temp_15_GOTM' = wtr_15$GOTM$wtr_20 - wtr_15$GOTM$wtr_2,
                              
                              'temp_2_GOTM' = wtr_2$GOTM$wtr_20 - wtr_2$GOTM$wtr_2,
                              
                              'temp_25_GOTM' = wtr_25$GOTM$wtr_20 - wtr_25$GOTM$wtr_2,
                              
                              'temp_3_GOTM' = wtr_3$GOTM$wtr_20 - wtr_3$GOTM$wtr_2,
                              
                              'temp_35_GOTM' = wtr_35$GOTM$wtr_20 - wtr_35$GOTM$wtr_2,
                              
                              'temp_4_GOTM' = wtr_4$GOTM$wtr_20 - wtr_4$GOTM$wtr_2,
                              
                              'temp_45_GOTM' = wtr_45$GOTM$wtr_20 - wtr_45$GOTM$wtr_2,
                              
                              'temp_5_GOTM' = wtr_5$GOTM$wtr_20 - wtr_5$GOTM$wtr_2,
                              
                              'temp_10_GOTM' = wtr_10$GOTM$wtr_20 - wtr_10$GOTM$wtr_2
                              
    )
    df_wtr_GOTM$id = lake.id
    
    df_ice_GOTM<- data.frame('datetime' = ice_null$GOTM$time,
                             'ice_null_GOTM' = ice_null$GOTM$ice_height,
                             'ice_01_GOTM' = ice_01$GOTM$ice_height,
                             'ice_05_GOTM' = ice_05$GOTM$ice_height,
                             'ice_1_GOTM' = ice_1$GOTM$ice_height,
                             'ice_15_GOTM' = ice_15$GOTM$ice_height,
                             'ice_2_GOTM' = ice_2$GOTM$ice_height,
                             'ice_25_GOTM' = ice_25$GOTM$ice_height,
                             'ice_3_GOTM' = ice_3$GOTM$ice_height,
                             'ice_35_GOTM' = ice_35$GOTM$ice_height,
                             'ice_4_GOTM' = ice_4$GOTM$ice_height,
                             'ice_45_GOTM' = ice_45$GOTM$ice_height,
                             'ice_5_GOTM' = ice_5$GOTM$ice_height,
                             'ice_10_GOTM' = ice_10$GOTM$ice_height
    )
    df_ice_GOTM$id = lake.id
    
    df_ssi_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                              'ssi_null_GOTM' = 
                                lapply(wtr_null, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability,
                              'ssi_01_GOTM' = 
                                lapply(wtr_01, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_05_GOTM' = 
                                lapply(wtr_05, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_1_GOTM' =
                                lapply(wtr_1, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_15_GOTM' = 
                                lapply(wtr_15, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_2_GOTM' = 
                                lapply(wtr_2, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_25_GOTM' = 
                                lapply(wtr_25, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_3_GOTM' = 
                                lapply(wtr_3, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability, 
                              'ssi_35_GOTM' = 
                                lapply(wtr_35, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability,
                              'ssi_4_GOTM' = 
                                lapply(wtr_4, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability,
                              'ssi_45_GOTM' = 
                                lapply(wtr_45, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability,
                              'ssi_5_GOTM' = 
                                lapply(wtr_5, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability,
                              'ssi_10_GOTM' = 
                                lapply(wtr_10, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GOTM$schmidt.stability
    )
    df_ssi_GOTM$id = lake.id
    
    df_ln_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                             'ln_null_GOTM' = 
                               lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_01_GOTM' = 
                               lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_05_GOTM' = 
                               lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_1_GOTM' = 
                               lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_15_GOTM' = 
                               lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_2_GOTM' = 
                               lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_25_GOTM' = 
                               lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_3_GOTM' = 
                               lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_35_GOTM' = 
                               lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_4_GOTM' = 
                               lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_45_GOTM' = 
                               lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_5_GOTM' = 
                               lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                             'ln_10_GOTM' = 
                               lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number
    )
    df_ln_GOTM$id = lake.id
    
    print(paste0('calculated all GOTM variables for ',lake.id))
    
    df_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                              'dens_null_Simstrat' = density_null$Simstrat$wtr_20 - density_null$Simstrat$wtr_2,
                              
                              'dens_01_Simstrat' = density_01$Simstrat$wtr_20 - density_01$Simstrat$wtr_2,
                              
                              'dens_05_Simstrat' = density_05$Simstrat$wtr_20 - density_05$Simstrat$wtr_2,
                              
                              'dens_1_Simstrat' = density_1$Simstrat$wtr_20 - density_1$Simstrat$wtr_2,
                              
                              'dens_15_Simstrat' = density_15$Simstrat$wtr_20 - density_15$Simstrat$wtr_2,
                              
                              'dens_2_Simstrat' = density_2$Simstrat$wtr_20 - density_2$Simstrat$wtr_2,
                              
                              'dens_25_Simstrat' = density_25$Simstrat$wtr_20 - density_25$Simstrat$wtr_2,
                              
                              'dens_3_Simstrat' = density_3$Simstrat$wtr_20 - density_3$Simstrat$wtr_2,
                              
                              'dens_35_Simstrat' = density_35$Simstrat$wtr_20 - density_35$Simstrat$wtr_2,
                              
                              'dens_4_Simstrat' = density_4$Simstrat$wtr_20 - density_4$Simstrat$wtr_2,
                              
                              'dens_45_Simstrat' = density_45$Simstrat$wtr_20 - density_45$Simstrat$wtr_2,
                              
                              'dens_5_Simstrat' = density_5$Simstrat$wtr_20 - density_5$Simstrat$wtr_2,
                              
                              'dens_10_Simstrat' = density_10$Simstrat$wtr_20 - density_10$Simstrat$wtr_2
                              
    )
    df_Simstrat$id = lake.id
    
    df_wtr_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                  'temp_null_Simstrat' = wtr_null$Simstrat$wtr_20 - wtr_null$Simstrat$wtr_2,
                                  
                                  'temp_01_Simstrat' = wtr_01$Simstrat$wtr_20 - wtr_01$Simstrat$wtr_2,
                                  
                                  'temp_05_Simstrat' = wtr_05$Simstrat$wtr_20 - wtr_05$Simstrat$wtr_2,
                                  
                                  'temp_1_Simstrat' = wtr_1$Simstrat$wtr_20 - wtr_1$Simstrat$wtr_2,
                                  
                                  'temp_15_Simstrat' = wtr_15$Simstrat$wtr_20 - wtr_15$Simstrat$wtr_2,
                                  
                                  'temp_2_Simstrat' = wtr_2$Simstrat$wtr_20 - wtr_2$Simstrat$wtr_2,
                                  
                                  'temp_25_Simstrat' = wtr_25$Simstrat$wtr_20 - wtr_25$Simstrat$wtr_2,
                                  
                                  'temp_3_Simstrat' = wtr_3$Simstrat$wtr_20 - wtr_3$Simstrat$wtr_2,
                                  
                                  'temp_35_Simstrat' = wtr_35$Simstrat$wtr_20 - wtr_35$Simstrat$wtr_2,
                                  
                                  'temp_4_Simstrat' = wtr_4$Simstrat$wtr_20 - wtr_4$Simstrat$wtr_2,
                                  
                                  'temp_45_Simstrat' = wtr_45$Simstrat$wtr_20 - wtr_45$Simstrat$wtr_2,
                                  
                                  'temp_5_Simstrat' = wtr_5$Simstrat$wtr_20 - wtr_5$Simstrat$wtr_2,
                                  
                                  'temp_10_Simstrat' = wtr_10$Simstrat$wtr_20 - wtr_10$Simstrat$wtr_2
                                  
    )
    df_wtr_Simstrat$id = lake.id
    
    df_ice_Simstrat<- data.frame('datetime' = ice_null$Simstrat$time,
                                 'ice_null_Simstrat' = ice_null$Simstrat$ice_height,
                                 'ice_01_Simstrat' = ice_01$Simstrat$ice_height,
                                 'ice_05_Simstrat' = ice_05$Simstrat$ice_height,
                                 'ice_1_Simstrat' = ice_1$Simstrat$ice_height,
                                 'ice_15_Simstrat' = ice_15$Simstrat$ice_height,
                                 'ice_2_Simstrat' = ice_2$Simstrat$ice_height,
                                 'ice_25_Simstrat' = ice_25$Simstrat$ice_height,
                                 'ice_3_Simstrat' = ice_3$Simstrat$ice_height,
                                 'ice_35_Simstrat' = ice_35$Simstrat$ice_height,
                                 'ice_4_Simstrat' = ice_4$Simstrat$ice_height,
                                 'ice_45_Simstrat' = ice_45$Simstrat$ice_height,
                                 'ice_5_Simstrat' = ice_5$Simstrat$ice_height,
                                 'ice_10_Simstrat' = ice_10$Simstrat$ice_height
    )
    df_ice_Simstrat$id = lake.id
    
    df_ssi_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                  'ssi_null_Simstrat' = 
                                    lapply(wtr_null, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability,
                                  'ssi_01_Simstrat' = 
                                    lapply(wtr_01, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_05_Simstrat' = 
                                    lapply(wtr_05, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_1_Simstrat' =
                                    lapply(wtr_1, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_15_Simstrat' = 
                                    lapply(wtr_15, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_2_Simstrat' = 
                                    lapply(wtr_2, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_25_Simstrat' = 
                                    lapply(wtr_25, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_3_Simstrat' = 
                                    lapply(wtr_3, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability, 
                                  'ssi_35_Simstrat' = 
                                    lapply(wtr_35, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability,
                                  'ssi_4_Simstrat' = 
                                    lapply(wtr_4, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability,
                                  'ssi_45_Simstrat' = 
                                    lapply(wtr_45, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability,
                                  'ssi_5_Simstrat' = 
                                    lapply(wtr_5, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability,
                                  'ssi_10_Simstrat' = 
                                    lapply(wtr_10, function(x) {
                                      ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                    })$Simstrat$schmidt.stability
    )
    df_ssi_Simstrat$id = lake.id
    
    df_ln_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                 'ln_null_Simstrat' = 
                                   lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_01_Simstrat' = 
                                   lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_05_Simstrat' = 
                                   lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_1_Simstrat' = 
                                   lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_15_Simstrat' = 
                                   lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_2_Simstrat' = 
                                   lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_25_Simstrat' = 
                                   lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_3_Simstrat' = 
                                   lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_35_Simstrat' = 
                                   lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_4_Simstrat' = 
                                   lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_45_Simstrat' = 
                                   lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_5_Simstrat' = 
                                   lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                 'ln_10_Simstrat' = 
                                   lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number
    )
    df_ln_Simstrat$id = lake.id
    
    print(paste0('calculated all Simstrat variables for ',lake.id))
    
  } else {
    df2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                      'dens_null' = apply(cbind(density_null$GLM$wtr_20 - density_null$GLM$wtr_2,
                                                density_null$GOTM$wtr_20 - density_null$GOTM$wtr_2,
                                                density_null$Simstrat$wtr_20 - density_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_01' = apply(cbind(density_01$GLM$wtr_20 - density_01$GLM$wtr_2,
                                              density_01$GOTM$wtr_20 - density_01$GOTM$wtr_2,
                                              density_01$Simstrat$wtr_20 - density_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_05' = apply(cbind(density_05$GLM$wtr_20 - density_05$GLM$wtr_2,
                                              density_05$GOTM$wtr_20 - density_05$GOTM$wtr_2,
                                              density_05$Simstrat$wtr_20 - density_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_1' = apply(cbind(density_1$GLM$wtr_20 - density_1$GLM$wtr_2,
                                             density_1$GOTM$wtr_20 - density_1$GOTM$wtr_2,
                                             density_1$Simstrat$wtr_20 - density_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_15' = apply(cbind(density_15$GLM$wtr_20 - density_15$GLM$wtr_2,
                                              density_15$GOTM$wtr_20 - density_15$GOTM$wtr_2,
                                              density_15$Simstrat$wtr_20 - density_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_2' = apply(cbind(density_2$GLM$wtr_20 - density_2$GLM$wtr_2,
                                             density_2$GOTM$wtr_20 - density_2$GOTM$wtr_2,
                                             density_2$Simstrat$wtr_20 - density_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_25' = apply(cbind(density_25$GLM$wtr_20 - density_25$GLM$wtr_2,
                                              density_25$GOTM$wtr_20 - density_25$GOTM$wtr_2,
                                              density_25$Simstrat$wtr_20 - density_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_3' = apply(cbind(density_3$GLM$wtr_20 - density_3$GLM$wtr_2,
                                             density_3$GOTM$wtr_20 - density_3$GOTM$wtr_2,
                                             density_3$Simstrat$wtr_20 - density_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_35' = apply(cbind(density_35$GLM$wtr_20 - density_35$GLM$wtr_2,
                                              density_35$GOTM$wtr_20 - density_35$GOTM$wtr_2,
                                              density_35$Simstrat$wtr_20 - density_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_4' = apply(cbind(density_4$GLM$wtr_20 - density_4$GLM$wtr_2,
                                             density_4$GOTM$wtr_20 - density_4$GOTM$wtr_2,
                                             density_4$Simstrat$wtr_20 - density_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_45' = apply(cbind(density_45$GLM$wtr_20 - density_45$GLM$wtr_2,
                                              density_45$GOTM$wtr_20 - density_45$GOTM$wtr_2,
                                              density_45$Simstrat$wtr_20 - density_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_5' = apply(cbind(density_5$GLM$wtr_20 - density_5$GLM$wtr_2,
                                             density_5$GOTM$wtr_20 - density_5$GOTM$wtr_2,
                                             density_5$Simstrat$wtr_20 - density_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                      'dens_10' = apply(cbind(density_10$GLM$wtr_20 - density_10$GLM$wtr_2,
                                              density_10$GOTM$wtr_20 - density_10$GOTM$wtr_2,
                                              density_10$Simstrat$wtr_20 - density_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
    )
    df2$id = lake.id
    
    df_wtr2 <- data.frame('datetime' = wtr_null$GLM$datetime,
                          'temp_null' = apply(cbind(wtr_null$GLM$wtr_20 - wtr_null$GLM$wtr_2,
                                                    wtr_null$GOTM$wtr_20 - wtr_null$GOTM$wtr_2,
                                                    wtr_null$Simstrat$wtr_20 - wtr_null$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_01' = apply(cbind(wtr_01$GLM$wtr_20 - wtr_01$GLM$wtr_2,
                                                  wtr_01$GOTM$wtr_20 - wtr_01$GOTM$wtr_2,
                                                  wtr_01$Simstrat$wtr_20 - wtr_01$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_05' = apply(cbind(wtr_05$GLM$wtr_20 - wtr_05$GLM$wtr_2,
                                                  wtr_05$GOTM$wtr_20 - wtr_05$GOTM$wtr_2,
                                                  wtr_05$Simstrat$wtr_20 - wtr_05$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_1' = apply(cbind(wtr_1$GLM$wtr_20 - wtr_1$GLM$wtr_2,
                                                 wtr_1$GOTM$wtr_20 - wtr_1$GOTM$wtr_2,
                                                 wtr_1$Simstrat$wtr_20 - wtr_1$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_15' = apply(cbind(wtr_15$GLM$wtr_20 - wtr_15$GLM$wtr_2,
                                                  wtr_15$GOTM$wtr_20 - wtr_15$GOTM$wtr_2,
                                                  wtr_15$Simstrat$wtr_20 - wtr_15$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_2' = apply(cbind(wtr_2$GLM$wtr_20 - wtr_2$GLM$wtr_2,
                                                 wtr_2$GOTM$wtr_20 - wtr_2$GOTM$wtr_2,
                                                 wtr_2$Simstrat$wtr_20 - wtr_2$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_25' = apply(cbind(wtr_25$GLM$wtr_20 - wtr_25$GLM$wtr_2,
                                                  wtr_25$GOTM$wtr_20 - wtr_25$GOTM$wtr_2,
                                                  wtr_25$Simstrat$wtr_20 - wtr_25$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_3' = apply(cbind(wtr_3$GLM$wtr_20 - wtr_3$GLM$wtr_2,
                                                 wtr_3$GOTM$wtr_20 - wtr_3$GOTM$wtr_2,
                                                 wtr_3$Simstrat$wtr_20 - wtr_3$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_35' = apply(cbind(wtr_35$GLM$wtr_20 - wtr_35$GLM$wtr_2,
                                                  wtr_35$GOTM$wtr_20 - wtr_35$GOTM$wtr_2,
                                                  wtr_35$Simstrat$wtr_20 - wtr_35$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_4' = apply(cbind(wtr_4$GLM$wtr_20 - wtr_4$GLM$wtr_2,
                                                 wtr_4$GOTM$wtr_20 - wtr_4$GOTM$wtr_2,
                                                 wtr_4$Simstrat$wtr_20 - wtr_4$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_45' = apply(cbind(wtr_45$GLM$wtr_20 - wtr_45$GLM$wtr_2,
                                                  wtr_45$GOTM$wtr_20 - wtr_45$GOTM$wtr_2,
                                                  wtr_45$Simstrat$wtr_20 - wtr_45$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_5' = apply(cbind(wtr_5$GLM$wtr_20 - wtr_5$GLM$wtr_2,
                                                 wtr_5$GOTM$wtr_20 - wtr_5$GOTM$wtr_2,
                                                 wtr_5$Simstrat$wtr_20 - wtr_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                          'temp_10' = apply(cbind(wtr_10$GLM$wtr_20 - wtr_10$GLM$wtr_2,
                                                  wtr_10$GOTM$wtr_20 - wtr_10$GOTM$wtr_2,
                                                  wtr_10$Simstrat$wtr_20 - wtr_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
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
                        'ln_35' = apply(cbind(
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
    
    print(paste0('calculated all scenario variables for ',lake.id))
    
    df2_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                          'dens_null_GLM' = density_null$GLM$wtr_20 - density_null$GLM$wtr_2,
                          
                          'dens_01_GLM' = density_01$GLM$wtr_20 - density_01$GLM$wtr_2,
                          
                          'dens_05_GLM' = density_05$GLM$wtr_20 - density_05$GLM$wtr_2,
                          
                          'dens_1_GLM' = density_1$GLM$wtr_20 - density_1$GLM$wtr_2,
                          
                          'dens_15_GLM' = density_15$GLM$wtr_20 - density_15$GLM$wtr_2,
                          
                          'dens_2_GLM' = density_2$GLM$wtr_20 - density_2$GLM$wtr_2,
                          
                          'dens_25_GLM' = density_25$GLM$wtr_20 - density_25$GLM$wtr_2,
                          
                          'dens_3_GLM' = density_3$GLM$wtr_20 - density_3$GLM$wtr_2,
                          
                          'dens_35_GLM' = density_35$GLM$wtr_20 - density_35$GLM$wtr_2,
                          
                          'dens_4_GLM' = density_4$GLM$wtr_20 - density_4$GLM$wtr_2,
                          
                          'dens_45_GLM' = density_45$GLM$wtr_20 - density_45$GLM$wtr_2,
                          
                          'dens_5_GLM' = density_5$GLM$wtr_20 - density_5$GLM$wtr_2,
                          
                          'dens_10_GLM' = density_10$GLM$wtr_20 - density_10$GLM$wtr_2
                          
    )
    df2_GLM$id = lake.id
    
    df2_wtr_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                              'temp_null_GLM' = wtr_null$GLM$wtr_20 - wtr_null$GLM$wtr_2,
                              
                              'temp_01_GLM' = wtr_01$GLM$wtr_20 - wtr_01$GLM$wtr_2,
                              
                              'temp_05_GLM' = wtr_05$GLM$wtr_20 - wtr_05$GLM$wtr_2,
                              
                              'temp_1_GLM' = wtr_1$GLM$wtr_20 - wtr_1$GLM$wtr_2,
                              
                              'temp_15_GLM' = wtr_15$GLM$wtr_20 - wtr_15$GLM$wtr_2,
                              
                              'temp_2_GLM' = wtr_2$GLM$wtr_20 - wtr_2$GLM$wtr_2,
                              
                              'temp_25_GLM' = wtr_25$GLM$wtr_20 - wtr_25$GLM$wtr_2,
                              
                              'temp_3_GLM' = wtr_3$GLM$wtr_20 - wtr_3$GLM$wtr_2,
                              
                              'temp_35_GLM' = wtr_35$GLM$wtr_20 - wtr_35$GLM$wtr_2,
                              
                              'temp_4_GLM' = wtr_4$GLM$wtr_20 - wtr_4$GLM$wtr_2,
                              
                              'temp_45_GLM' = wtr_45$GLM$wtr_20 - wtr_45$GLM$wtr_2,
                              
                              'temp_5_GLM' = wtr_5$GLM$wtr_20 - wtr_5$GLM$wtr_2,
                              
                              'temp_10_GLM' = wtr_10$GLM$wtr_20 - wtr_10$GLM$wtr_2
                              
    )
    df2_wtr_GLM$id = lake.id
    
    df2_ice_GLM<- data.frame('datetime' = ice_null$GLM$time,
                             'ice_null_GLM' = ice_null$GLM$ice_height,
                             'ice_01_GLM' = ice_01$GLM$ice_height,
                             'ice_05_GLM' = ice_05$GLM$ice_height,
                             'ice_1_GLM' = ice_1$GLM$ice_height,
                             'ice_15_GLM' = ice_15$GLM$ice_height,
                             'ice_2_GLM' = ice_2$GLM$ice_height,
                             'ice_25_GLM' = ice_25$GLM$ice_height,
                             'ice_3_GLM' = ice_3$GLM$ice_height,
                             'ice_35_GLM' = ice_35$GLM$ice_height,
                             'ice_4_GLM' = ice_4$GLM$ice_height,
                             'ice_45_GLM' = ice_45$GLM$ice_height,
                             'ice_5_GLM' = ice_5$GLM$ice_height,
                             'ice_10_GLM' = ice_10$GLM$ice_height
    )
    df2_ice_GLM$id = lake.id
    
    df2_ssi_GLM <- data.frame('datetime' = wtr_null$GLM$datetime,
                              'ssi_null_GLM' = 
                                lapply(wtr_null, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability,
                              'ssi_01_GLM' = 
                                lapply(wtr_01, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_05_GLM' = 
                                lapply(wtr_05, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_1_GLM' =
                                lapply(wtr_1, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_15_GLM' = 
                                lapply(wtr_15, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_2_GLM' = 
                                lapply(wtr_2, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_25_GLM' = 
                                lapply(wtr_25, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_3_GLM' = 
                                lapply(wtr_3, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability, 
                              'ssi_35_GLM' = 
                                lapply(wtr_35, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability,
                              'ssi_4_GLM' = 
                                lapply(wtr_4, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability,
                              'ssi_45_GLM' = 
                                lapply(wtr_45, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability,
                              'ssi_5_GLM' = 
                                lapply(wtr_5, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability,
                              'ssi_10_GLM' = 
                                lapply(wtr_10, function(x) {
                                  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                })$GLM$schmidt.stability
    )
    df2_ssi_GLM$id = lake.id
    
    for (i in 2:ncol(wtr_null$GLM)){
      if(any(is.na(wtr_null$GLM[-c(1),i]))){
        dummy_null = wtr_null
        dummy_null$GLM = wtr_null$GLM[,1:(i-1)]
        
        dummy_01 = wtr_01
        dummy_01$GLM = wtr_01$GLM[,1:(i-1)]
        
        dummy_05 = wtr_05
        dummy_05$GLM = wtr_05$GLM[,1:(i-1)]
        
        dummy_1 = wtr_1
        dummy_1$GLM = wtr_1$GLM[,1:(i-1)]
        
        dummy_15 = wtr_15
        dummy_15$GLM = wtr_15$GLM[,1:(i-1)]
        
        dummy_2 = wtr_2
        dummy_2$GLM = wtr_2$GLM[,1:(i-1)]
        
        dummy_25 = wtr_25
        dummy_25$GLM = wtr_25$GLM[,1:(i-1)]
        
        dummy_3 = wtr_3
        dummy_3$GLM = wtr_3$GLM[,1:(i-1)]
        
        dummy_35 = wtr_35
        dummy_35$GLM = wtr_35$GLM[,1:(i-1)]
        
        dummy_4 = wtr_4
        dummy_4$GLM = wtr_4$GLM[,1:(i-1)]
        
        dummy_45 = wtr_45
        dummy_45$GLM = wtr_45$GLM[,1:(i-1)]
        
        dummy_5 = wtr_5
        dummy_5$GLM = wtr_5$GLM[,1:(i-1)]
        
        dummy_10 = wtr_10
        dummy_10$GLM = wtr_10$GLM[,1:(i-1)]
        break
      }
    }
    df2_ln_GLM <- data.frame('datetime' = dummy_null$GLM$datetime,
                             'ln_null_GLM' = 
                               lapply(dummy_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_01_GLM' = 
                               lapply(dummy_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_05_GLM' = 
                               lapply(dummy_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_1_GLM' = 
                               lapply(dummy_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_15_GLM' = 
                               lapply(dummy_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_2_GLM' = 
                               lapply(dummy_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_25_GLM' = 
                               lapply(dummy_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_3_GLM' = 
                               lapply(dummy_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_35_GLM' = 
                               lapply(dummy_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_4_GLM' = 
                               lapply(dummy_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_45_GLM' = 
                               lapply(dummy_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_5_GLM' = 
                               lapply(dummy_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number,
                             'ln_10_GLM' = 
                               lapply(dummy_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GLM$lake.number
    )
    df2_ln_GLM$id = lake.id
    
    df_GLM = rbind(df_GLM, df2_GLM)
    df_wtr_GLM = rbind(df_wtr_GLM, df2_wtr_GLM)
    df_ssi_GLM = rbind(df_ssi_GLM, df2_ssi_GLM)
    df_ice_GLM <- rbind(df_ice_GLM, df2_ice_GLM)
    df_ln_GLM <- rbind(df_ln_GLM, df2_ln_GLM)
    
    print(paste0('calculated all GLM variables for ',lake.id))
    
    df2_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                           'dens_null_GOTM' = density_null$GOTM$wtr_20 - density_null$GOTM$wtr_2,
                           
                           'dens_01_GOTM' = density_01$GOTM$wtr_20 - density_01$GOTM$wtr_2,
                           
                           'dens_05_GOTM' = density_05$GOTM$wtr_20 - density_05$GOTM$wtr_2,
                           
                           'dens_1_GOTM' = density_1$GOTM$wtr_20 - density_1$GOTM$wtr_2,
                           
                           'dens_15_GOTM' = density_15$GOTM$wtr_20 - density_15$GOTM$wtr_2,
                           
                           'dens_2_GOTM' = density_2$GOTM$wtr_20 - density_2$GOTM$wtr_2,
                           
                           'dens_25_GOTM' = density_25$GOTM$wtr_20 - density_25$GOTM$wtr_2,
                           
                           'dens_3_GOTM' = density_3$GOTM$wtr_20 - density_3$GOTM$wtr_2,
                           
                           'dens_35_GOTM' = density_35$GOTM$wtr_20 - density_35$GOTM$wtr_2,
                           
                           'dens_4_GOTM' = density_4$GOTM$wtr_20 - density_4$GOTM$wtr_2,
                           
                           'dens_45_GOTM' = density_45$GOTM$wtr_20 - density_45$GOTM$wtr_2,
                           
                           'dens_5_GOTM' = density_5$GOTM$wtr_20 - density_5$GOTM$wtr_2,
                           
                           'dens_10_GOTM' = density_10$GOTM$wtr_20 - density_10$GOTM$wtr_2
                           
    )
    df2_GOTM$id = lake.id
    
    df2_wtr_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                               'temp_null_GOTM' = wtr_null$GOTM$wtr_20 - wtr_null$GOTM$wtr_2,
                               
                               'temp_01_GOTM' = wtr_01$GOTM$wtr_20 - wtr_01$GOTM$wtr_2,
                               
                               'temp_05_GOTM' = wtr_05$GOTM$wtr_20 - wtr_05$GOTM$wtr_2,
                               
                               'temp_1_GOTM' = wtr_1$GOTM$wtr_20 - wtr_1$GOTM$wtr_2,
                               
                               'temp_15_GOTM' = wtr_15$GOTM$wtr_20 - wtr_15$GOTM$wtr_2,
                               
                               'temp_2_GOTM' = wtr_2$GOTM$wtr_20 - wtr_2$GOTM$wtr_2,
                               
                               'temp_25_GOTM' = wtr_25$GOTM$wtr_20 - wtr_25$GOTM$wtr_2,
                               
                               'temp_3_GOTM' = wtr_3$GOTM$wtr_20 - wtr_3$GOTM$wtr_2,
                               
                               'temp_35_GOTM' = wtr_35$GOTM$wtr_20 - wtr_35$GOTM$wtr_2,
                               
                               'temp_4_GOTM' = wtr_4$GOTM$wtr_20 - wtr_4$GOTM$wtr_2,
                               
                               'temp_45_GOTM' = wtr_45$GOTM$wtr_20 - wtr_45$GOTM$wtr_2,
                               
                               'temp_5_GOTM' = wtr_5$GOTM$wtr_20 - wtr_5$GOTM$wtr_2,
                               
                               'temp_10_GOTM' = wtr_10$GOTM$wtr_20 - wtr_10$GOTM$wtr_2
                               
    )
    df2_wtr_GOTM$id = lake.id
    
    df2_ice_GOTM<- data.frame('datetime' = ice_null$GOTM$time,
                              'ice_null_GOTM' = ice_null$GOTM$ice_height,
                              'ice_01_GOTM' = ice_01$GOTM$ice_height,
                              'ice_05_GOTM' = ice_05$GOTM$ice_height,
                              'ice_1_GOTM' = ice_1$GOTM$ice_height,
                              'ice_15_GOTM' = ice_15$GOTM$ice_height,
                              'ice_2_GOTM' = ice_2$GOTM$ice_height,
                              'ice_25_GOTM' = ice_25$GOTM$ice_height,
                              'ice_3_GOTM' = ice_3$GOTM$ice_height,
                              'ice_35_GOTM' = ice_35$GOTM$ice_height,
                              'ice_4_GOTM' = ice_4$GOTM$ice_height,
                              'ice_45_GOTM' = ice_45$GOTM$ice_height,
                              'ice_5_GOTM' = ice_5$GOTM$ice_height,
                              'ice_10_GOTM' = ice_10$GOTM$ice_height
    )
    df2_ice_GOTM$id = lake.id
    
    df2_ssi_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                               'ssi_null_GOTM' = 
                                 lapply(wtr_null, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability,
                               'ssi_01_GOTM' = 
                                 lapply(wtr_01, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_05_GOTM' = 
                                 lapply(wtr_05, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_1_GOTM' =
                                 lapply(wtr_1, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_15_GOTM' = 
                                 lapply(wtr_15, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_2_GOTM' = 
                                 lapply(wtr_2, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_25_GOTM' = 
                                 lapply(wtr_25, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_3_GOTM' = 
                                 lapply(wtr_3, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability, 
                               'ssi_35_GOTM' = 
                                 lapply(wtr_35, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability,
                               'ssi_4_GOTM' = 
                                 lapply(wtr_4, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability,
                               'ssi_45_GOTM' = 
                                 lapply(wtr_45, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability,
                               'ssi_5_GOTM' = 
                                 lapply(wtr_5, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability,
                               'ssi_10_GOTM' = 
                                 lapply(wtr_10, function(x) {
                                   ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                 })$GOTM$schmidt.stability
    )
    df2_ssi_GOTM$id = lake.id
    
    df2_ln_GOTM <- data.frame('datetime' = wtr_null$GOTM$datetime,
                              'ln_null_GOTM' = 
                                lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_01_GOTM' = 
                                lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_05_GOTM' = 
                                lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_1_GOTM' = 
                                lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_15_GOTM' = 
                                lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_2_GOTM' = 
                                lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_25_GOTM' = 
                                lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_3_GOTM' = 
                                lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_35_GOTM' = 
                                lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_4_GOTM' = 
                                lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_45_GOTM' = 
                                lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_5_GOTM' = 
                                lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number,
                              'ln_10_GOTM' = 
                                lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$GOTM$lake.number
    )
    df2_ln_GOTM$id = lake.id
    
    df_GOTM = rbind(df_GOTM, df2_GOTM)
    df_wtr_GOTM = rbind(df_wtr_GOTM, df2_wtr_GOTM)
    df_ssi_GOTM = rbind(df_ssi_GOTM, df2_ssi_GOTM)
    df_ice_GOTM <- rbind(df_ice_GOTM, df2_ice_GOTM)
    df_ln_GOTM <- rbind(df_ln_GOTM, df2_ln_GOTM)
    
    print(paste0('calculated all GOTM variables for ',lake.id))
    
    df2_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                               'dens_null_Simstrat' = density_null$Simstrat$wtr_20 - density_null$Simstrat$wtr_2,
                               
                               'dens_01_Simstrat' = density_01$Simstrat$wtr_20 - density_01$Simstrat$wtr_2,
                               
                               'dens_05_Simstrat' = density_05$Simstrat$wtr_20 - density_05$Simstrat$wtr_2,
                               
                               'dens_1_Simstrat' = density_1$Simstrat$wtr_20 - density_1$Simstrat$wtr_2,
                               
                               'dens_15_Simstrat' = density_15$Simstrat$wtr_20 - density_15$Simstrat$wtr_2,
                               
                               'dens_2_Simstrat' = density_2$Simstrat$wtr_20 - density_2$Simstrat$wtr_2,
                               
                               'dens_25_Simstrat' = density_25$Simstrat$wtr_20 - density_25$Simstrat$wtr_2,
                               
                               'dens_3_Simstrat' = density_3$Simstrat$wtr_20 - density_3$Simstrat$wtr_2,
                               
                               'dens_35_Simstrat' = density_35$Simstrat$wtr_20 - density_35$Simstrat$wtr_2,
                               
                               'dens_4_Simstrat' = density_4$Simstrat$wtr_20 - density_4$Simstrat$wtr_2,
                               
                               'dens_45_Simstrat' = density_45$Simstrat$wtr_20 - density_45$Simstrat$wtr_2,
                               
                               'dens_5_Simstrat' = density_5$Simstrat$wtr_20 - density_5$Simstrat$wtr_2,
                               
                               'dens_10_Simstrat' = density_10$Simstrat$wtr_20 - density_10$Simstrat$wtr_2
                               
    )
    df2_Simstrat$id = lake.id
    
    df2_wtr_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                   'temp_null_Simstrat' = wtr_null$Simstrat$wtr_20 - wtr_null$Simstrat$wtr_2,
                                   
                                   'temp_01_Simstrat' = wtr_01$Simstrat$wtr_20 - wtr_01$Simstrat$wtr_2,
                                   
                                   'temp_05_Simstrat' = wtr_05$Simstrat$wtr_20 - wtr_05$Simstrat$wtr_2,
                                   
                                   'temp_1_Simstrat' = wtr_1$Simstrat$wtr_20 - wtr_1$Simstrat$wtr_2,
                                   
                                   'temp_15_Simstrat' = wtr_15$Simstrat$wtr_20 - wtr_15$Simstrat$wtr_2,
                                   
                                   'temp_2_Simstrat' = wtr_2$Simstrat$wtr_20 - wtr_2$Simstrat$wtr_2,
                                   
                                   'temp_25_Simstrat' = wtr_25$Simstrat$wtr_20 - wtr_25$Simstrat$wtr_2,
                                   
                                   'temp_3_Simstrat' = wtr_3$Simstrat$wtr_20 - wtr_3$Simstrat$wtr_2,
                                   
                                   'temp_35_Simstrat' = wtr_35$Simstrat$wtr_20 - wtr_35$Simstrat$wtr_2,
                                   
                                   'temp_4_Simstrat' = wtr_4$Simstrat$wtr_20 - wtr_4$Simstrat$wtr_2,
                                   
                                   'temp_45_Simstrat' = wtr_45$Simstrat$wtr_20 - wtr_45$Simstrat$wtr_2,
                                   
                                   'temp_5_Simstrat' = wtr_5$Simstrat$wtr_20 - wtr_5$Simstrat$wtr_2,
                                   
                                   'temp_10_Simstrat' = wtr_10$Simstrat$wtr_20 - wtr_10$Simstrat$wtr_2
                                   
    )
    df2_wtr_Simstrat$id = lake.id
    
    df2_ice_Simstrat<- data.frame('datetime' = ice_null$Simstrat$time,
                                  'ice_null_Simstrat' = ice_null$Simstrat$ice_height,
                                  'ice_01_Simstrat' = ice_01$Simstrat$ice_height,
                                  'ice_05_Simstrat' = ice_05$Simstrat$ice_height,
                                  'ice_1_Simstrat' = ice_1$Simstrat$ice_height,
                                  'ice_15_Simstrat' = ice_15$Simstrat$ice_height,
                                  'ice_2_Simstrat' = ice_2$Simstrat$ice_height,
                                  'ice_25_Simstrat' = ice_25$Simstrat$ice_height,
                                  'ice_3_Simstrat' = ice_3$Simstrat$ice_height,
                                  'ice_35_Simstrat' = ice_35$Simstrat$ice_height,
                                  'ice_4_Simstrat' = ice_4$Simstrat$ice_height,
                                  'ice_45_Simstrat' = ice_45$Simstrat$ice_height,
                                  'ice_5_Simstrat' = ice_5$Simstrat$ice_height,
                                  'ice_10_Simstrat' = ice_10$Simstrat$ice_height
    )
    df2_ice_Simstrat$id = lake.id
    
    df2_ssi_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                   'ssi_null_Simstrat' = 
                                     lapply(wtr_null, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability,
                                   'ssi_01_Simstrat' = 
                                     lapply(wtr_01, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_05_Simstrat' = 
                                     lapply(wtr_05, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_1_Simstrat' =
                                     lapply(wtr_1, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_15_Simstrat' = 
                                     lapply(wtr_15, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_2_Simstrat' = 
                                     lapply(wtr_2, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_25_Simstrat' = 
                                     lapply(wtr_25, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_3_Simstrat' = 
                                     lapply(wtr_3, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability, 
                                   'ssi_35_Simstrat' = 
                                     lapply(wtr_35, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability,
                                   'ssi_4_Simstrat' = 
                                     lapply(wtr_4, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability,
                                   'ssi_45_Simstrat' = 
                                     lapply(wtr_45, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability,
                                   'ssi_5_Simstrat' = 
                                     lapply(wtr_5, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability,
                                   'ssi_10_Simstrat' = 
                                     lapply(wtr_10, function(x) {
                                       ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
                                     })$Simstrat$schmidt.stability
    )
    df2_ssi_Simstrat$id = lake.id
    
    df2_ln_Simstrat <- data.frame('datetime' = wtr_null$Simstrat$datetime,
                                  'ln_null_Simstrat' = 
                                    lapply(wtr_null, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_01_Simstrat' = 
                                    lapply(wtr_01, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_05_Simstrat' = 
                                    lapply(wtr_05, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_1_Simstrat' = 
                                    lapply(wtr_1, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_15_Simstrat' = 
                                    lapply(wtr_15, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_2_Simstrat' = 
                                    lapply(wtr_2, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_25_Simstrat' = 
                                    lapply(wtr_25, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_3_Simstrat' = 
                                    lapply(wtr_3, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_35_Simstrat' = 
                                    lapply(wtr_35, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_4_Simstrat' = 
                                    lapply(wtr_4, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_45_Simstrat' = 
                                    lapply(wtr_45, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_5_Simstrat' = 
                                    lapply(wtr_5, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number,
                                  'ln_10_Simstrat' = 
                                    lapply(wtr_10, function(x) {ts.lake.number(wtr = x, wnd = wind, wnd.height = 10, bathy =  bathy)})$Simstrat$lake.number
    )
    df2_ln_Simstrat$id = lake.id
    
    df_Simstrat = rbind(df_Simstrat, df2_Simstrat)
    df_wtr_Simstrat = rbind(df_wtr_Simstrat, df2_wtr_Simstrat)
    df_ssi_Simstrat = rbind(df_ssi_Simstrat, df2_ssi_Simstrat)
    df_ice_Simstrat <- rbind(df_ice_Simstrat, df2_ice_Simstrat)
    df_ln_Simstrat <- rbind(df_ln_Simstrat, df2_ln_Simstrat)
    
    print(paste0('calculated all Simstrat variables for ',lake.id))
  }
  
}

write.csv(x = df, file = 'output/density.csv', quote = F, row.names = F)
write.csv(x = df_wtr, file = 'output/wtemp.csv', quote = F, row.names = F)
write.csv(x = df_ssi, file = 'output/ssi.csv', quote = F, row.names = F)
write.csv(x = df_ice, file = 'output/ice.csv', quote = F, row.names = F)
write.csv(x = df_ln, file = 'output/lakenumber.csv', quote = F, row.names = F)

write.csv(x = df_GLM, file = 'output/density_GLM.csv', quote = F, row.names = F)
write.csv(x = df_wtr_GLM, file = 'output/wtemp_GLM.csv', quote = F, row.names = F)
write.csv(x = df_ssi_GLM, file = 'output/ssi_GLM.csv', quote = F, row.names = F)
write.csv(x = df_ice_GLM, file = 'output/ice_GLM.csv', quote = F, row.names = F)
write.csv(x = df_ln_GLM, file = 'output/lakenumber_GLM.csv', quote = F, row.names = F)

write.csv(x = df_GOTM, file = 'output/density_GOTM.csv', quote = F, row.names = F)
write.csv(x = df_wtr_GOTM, file = 'output/wtemp_GOTM.csv', quote = F, row.names = F)
write.csv(x = df_ssi_GOTM, file = 'output/ssi_GOTM.csv', quote = F, row.names = F)
write.csv(x = df_ice_GOTM, file = 'output/ice_GOTM.csv', quote = F, row.names = F)
write.csv(x = df_ln_GOTM, file = 'output/lakenumber_GOTM.csv', quote = F, row.names = F)

write.csv(x = df_Simstrat, file = 'output/density_Simstrat.csv', quote = F, row.names = F)
write.csv(x = df_wtr_Simstrat, file = 'output/wtemp_Simstrat.csv', quote = F, row.names = F)
write.csv(x = df_ssi_Simstrat, file = 'output/ssi_Simstrat.csv', quote = F, row.names = F)
write.csv(x = df_ice_Simstrat, file = 'output/ice_Simstrat.csv', quote = F, row.names = F)
write.csv(x = df_ln_Simstrat, file = 'output/lakenumber_Simstrat.csv', quote = F, row.names = F)

