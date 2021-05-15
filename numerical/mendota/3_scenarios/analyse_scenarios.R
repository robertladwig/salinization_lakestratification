setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Load libraries for post-processing
library(lubridate)
library(gotmtools)
library(ggplot2)
library(ggpubr)

bathy <- read.csv('1_null/LakeEnsemblR_bathymetry_standard.csv')
colnames(bathy) <- c("depths", "areas")

ncdf_null <- '1_null/output/ensemble_output.nc'
out_null <- load_var(ncdf = ncdf_null, var = "temp")
density_null <- load_var(ncdf = ncdf_null, var = "dens")
salt_null <- load_var(ncdf = ncdf_null, var = "salt")

ts.sch <- lapply(out_null, function(x) {
  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})
## Reshape to data.frame
df_null <- reshape2::melt(ts.sch, id.vars = 1)
colnames(df_null)[4] <- "model"

ncdf_10 <- '5_constantsalt+10/output/ensemble_output.nc'
out_10 <- load_var(ncdf = ncdf_10, var = "temp")
density_10 <- load_var(ncdf = ncdf_10, var = "dens")
salt_10 <- load_var(ncdf = ncdf_10, var = "salt")

ts.sch <- lapply(out_10, function(x) {
  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})
## Reshape to data.frame
df_10 <- reshape2::melt(ts.sch, id.vars = 1)
colnames(df_10)[4] <- "model"

df = data.frame('time' = df_null$datetime, 'doy' = yday(df_null$datetime), 'year' = year(df_null$datetime), 'month' = month(df_null$datetime),
                'GLM_null' = subset(df_null, model == 'GLM')$value, 'GLM_HighPSU' = subset(df_10, model == 'GLM')$value,
                'GOTM_null' = subset(df_null, model == 'GOTM')$value, 'GOTM_HighPSU' = subset(df_10, model == 'GOTM')$value,
                'Simstrat_null' = subset(df_null, model == 'Simstrat')$value, 'Simstrat_HighPSU' = subset(df_10, model == 'Simstrat')$value)
## plot results
g <- ggplot(df) +
  geom_line(aes(doy, GLM_null, colour = 'null')) +
  geom_line(aes(doy, GLM_HighPSU, colour = 'HighPSU')) +
  labs(y = "Schmidt stability (J/m2)") +
  facet_wrap(~ year) +
  ylim(-20,300) + ggtitle('GLM') +
  theme_light(); g
ggsave('output/GLM_schmidt.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')

g <- ggplot(df) +
  geom_line(aes(doy, GOTM_null, colour = 'null')) +
  geom_line(aes(doy, GOTM_HighPSU, colour = 'HighPSU')) +
  labs(y = "Schmidt stability (J/m2)") +
  facet_wrap(~ year) +
  ylim(-20,300) + ggtitle('GOTM') +
  theme_light(); g
ggsave('output/GOTM_schmidt.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')

g <- ggplot(df) +
  geom_line(aes(doy, Simstrat_null, colour = 'null')) +
  geom_line(aes(doy, Simstrat_HighPSU, colour = 'HighPSU')) +
  labs(y = "Schmidt stability (J/m2)") +
  facet_wrap(~ year) +
  ylim(-20,300) + ggtitle('Simstrat') +
  theme_light(); g
ggsave('output/Simstrat_schmidt.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')

df_dens = data.frame('time' = density_null$GLM$datetime, 'doy' = yday(density_null$GLM$datetime), 'year' = year(density_null$GLM$datetime), 'month' = month(density_null$GLM$datetime),
                     'GLM_null' = density_null$GLM$wtr_20 - density_null$GLM$wtr_2, 'GLM_HighPSU' = density_10$GLM$wtr_20 - density_10$GLM$wtr_2,
                     'GLM_null_sal' = apply(salt_null$GLM[,20:74],1, function(x) mean(x,na.rm = TRUE)), 'GLM_HighPSU_sal' = apply(salt_10$GLM[,20:74],1, function(x) mean(x,na.rm = TRUE)),
                     'GLM_null_temp' = out_null$GLM$wtr_20 - out_null$GLM$wtr_2, 'GLM_HighPSU_temp' = out_10$GLM$wtr_20 - out_10$GLM$wtr_2,
                     'GOTM_null' = density_null$GOTM$wtr_20 - density_null$GOTM$wtr_2, 'GOTM_HighPSU' = density_10$GOTM$wtr_20 - density_10$GOTM$wtr_2,
                     'GOTM_null_sal' = apply(salt_null$GOTM[,20:74],1, function(x) mean(x,na.rm = TRUE)), 'GOTM_HighPSU_sal' = apply(salt_10$GOTM[,20:74],1, function(x) mean(x,na.rm = TRUE)),
                     'GOTM_null_temp' = out_null$GOTM$wtr_20 - out_null$GOTM$wtr_2, 'GOTM_HighPSU_temp' = out_10$GOTM$wtr_20 - out_10$GOTM$wtr_2,
                     'Simstrat_null' = density_null$Simstrat$wtr_20 - density_null$Simstrat$wtr_2, 'Simstrat_HighPSU' = density_10$Simstrat$wtr_20 - density_10$Simstrat$wtr_2,
                     'Simstrat_null_sal' = apply(salt_null$Simstrat[,20:74],1, function(x) mean(x,na.rm = TRUE)), 'Simstrat_HighPSU_sal' = apply(salt_10$Simstrat[,20:74],1, function(x) mean(x,na.rm = TRUE)),
                     'Simstrat_null_temp' = out_null$Simstrat$wtr_20 - out_null$Simstrat$wtr_2, 'Simstrat_HighPSU_temp' = out_10$Simstrat$wtr_20 - out_10$Simstrat$wtr_2)

g <- ggplot(df_dens) +
  geom_line(aes(doy, GLM_null, colour = 'GLM', linetype = 'Null')) +
  geom_line(aes(doy, GLM_HighPSU, colour = 'GLM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, GOTM_null, colour = 'GOTM', linetype = 'Null')) +
  geom_line(aes(doy, GOTM_HighPSU, colour = 'GOTM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, Simstrat_null, colour = 'Simstrat', linetype = 'Null')) +
  geom_line(aes(doy, Simstrat_HighPSU, colour = 'Simstrat', linetype = 'HighPSU') ) +
  labs(y = "Density difference 20-2 m (kg/m3)") +
  facet_wrap(~ year) +
  ylim(-1,5)+
  theme_light(); g
ggsave('output/DensityDifferences.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')

g <- ggplot(df_dens) +
  geom_line(aes(doy, GLM_null_sal, colour = 'GLM', linetype = 'Null')) +
  geom_line(aes(doy, GLM_HighPSU_sal, colour = 'GLM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, GOTM_null_sal, colour = 'GOTM', linetype = 'Null')) +
  geom_line(aes(doy, GOTM_HighPSU_sal, colour = 'GOTM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, Simstrat_null_sal, colour = 'Simstrat', linetype = 'Null')) +
  geom_line(aes(doy, Simstrat_HighPSU_sal, colour = 'Simstrat', linetype = 'HighPSU') ) +
  labs(y = "Salinity 8 to 24 m (k/kg)") +
  facet_wrap(~ year) +
  ylim(0,7)+
  theme_light(); g
ggsave('output/HypolimnionSalt.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')

g <- ggplot(df_dens) +
  geom_line(aes(doy, GLM_null_temp, colour = 'GLM', linetype = 'Null')) +
  geom_line(aes(doy, GLM_HighPSU_temp, colour = 'GLM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, GOTM_null_temp, colour = 'GOTM', linetype = 'Null')) +
  geom_line(aes(doy, GOTM_HighPSU_temp, colour = 'GOTM', linetype = 'HighPSU') ) +
  geom_line(aes(doy, Simstrat_null_temp, colour = 'Simstrat', linetype = 'Null')) +
  geom_line(aes(doy, Simstrat_HighPSU_temp, colour = 'Simstrat', linetype = 'HighPSU') ) +
  labs(y = "WTR difference 20-2 m (deg C)") +
  facet_wrap(~ year) +
  ylim(-15,5)+
  theme_light(); g
ggsave('output/WTRDifferences.png', g,  dpi = 300,width = 384,height = 384, units = 'mm')


ncdf_null <- '1_null/output/ensemble_output.nc'
wtr_null <- load_var(ncdf = ncdf_null, var = "temp")
density_null <- load_var(ncdf = ncdf_null, var = "dens")
salt_null <- load_var(ncdf = ncdf_null, var = "salt")

ncdf_01 <- '2_constantsalt//output/ensemble_output.nc'
wtr_01 <- load_var(ncdf = ncdf_01, var = "temp")
density_01 <- load_var(ncdf = ncdf_01, var = "dens")
salt_01 <- load_var(ncdf = ncdf_01, var = "salt")

ncdf_05 <- '6_constantsalt+05/output/ensemble_output.nc'
wtr_05 <- load_var(ncdf = ncdf_05, var = "temp")
density_05 <- load_var(ncdf = ncdf_05, var = "dens")
salt_05 <- load_var(ncdf = ncdf_05, var = "salt")

ncdf_1 <- '7_constantsalt+1/output/ensemble_output.nc'
wtr_1 <- load_var(ncdf = ncdf_1, var = "temp")
density_1 <- load_var(ncdf = ncdf_1, var = "dens")
salt_1 <- load_var(ncdf = ncdf_1, var = "salt")

ncdf_15 <- '8_constantsalt+15/output/ensemble_output.nc'
wtr_15 <- load_var(ncdf = ncdf_15, var = "temp")
density_15 <- load_var(ncdf = ncdf_15, var = "dens")
salt_15 <- load_var(ncdf = ncdf_15, var = "salt")

ncdf_2 <- '3_constantsalt+2/output/ensemble_output.nc'
wtr_2 <- load_var(ncdf = ncdf_2, var = "temp")
density_2 <- load_var(ncdf = ncdf_2, var = "dens")
salt_2 <- load_var(ncdf = ncdf_2, var = "salt")

ncdf_5 <- '4_constantsalt+5/output/ensemble_output.nc'
wtr_5 <- load_var(ncdf = ncdf_5, var = "temp")
density_5 <- load_var(ncdf = ncdf_5, var = "dens")
salt_5 <- load_var(ncdf = ncdf_5, var = "salt")

ncdf_10 <- '5_constantsalt+10/output/ensemble_output.nc'
wtr_10 <- load_var(ncdf = ncdf_10, var = "temp")
density_10 <- load_var(ncdf = ncdf_10, var = "dens")
salt_10 <- load_var(ncdf = ncdf_10, var = "salt")

ncdf_25 <- '9_constantsalt+25/output/ensemble_output.nc'
wtr_25 <- load_var(ncdf = ncdf_25, var = "temp")
density_25 <- load_var(ncdf = ncdf_25, var = "dens")
salt_25 <- load_var(ncdf = ncdf_25, var = "salt")

ncdf_3 <- '10_constantsalt+3/output/ensemble_output.nc'
wtr_3 <- load_var(ncdf = ncdf_3, var = "temp")
density_3 <- load_var(ncdf = ncdf_3, var = "dens")
salt_3 <- load_var(ncdf = ncdf_3, var = "salt")

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
                 'dens_5' = apply(cbind(density_5$GLM$wtr_22 - density_5$GLM$wtr_2,
                                           density_5$GOTM$wtr_22 - density_5$GOTM$wtr_2,
                                           density_5$Simstrat$wtr_22 - density_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                 'dens_10' = apply(cbind(density_10$GLM$wtr_22 - density_10$GLM$wtr_2,
                                           density_10$GOTM$wtr_22 - density_10$GOTM$wtr_2,
                                           density_10$Simstrat$wtr_22 - density_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
                 )

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
                 'temp_5' = apply(cbind(wtr_5$GLM$wtr_22 - wtr_5$GLM$wtr_2,
                                        wtr_5$GOTM$wtr_22 - wtr_5$GOTM$wtr_2,
                                        wtr_5$Simstrat$wtr_22 - wtr_5$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE)),
                 'temp_10' = apply(cbind(wtr_10$GLM$wtr_22 - wtr_10$GLM$wtr_2,
                                         wtr_10$GOTM$wtr_22 - wtr_10$GOTM$wtr_2,
                                         wtr_10$Simstrat$wtr_22 - wtr_10$Simstrat$wtr_2), 1, function(x) mean(x, na.rm = TRUE))
)

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

m.df <- reshape2::melt(df, id = 'datetime')

g <- ggplot(m.df) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Density differences [g/kg]') +
  xlab('') +
  geom_hline(yintercept = 1.1, linetype = 'dashed') +
  theme_minimal(); g
ggsave('output/densitydiff.png', g,  dpi = 300,width = 250,height = 100, units = 'mm')

df_ssi[,2:ncol(df_ssi)] <- apply(as.matrix(df_ssi[,2:ncol(df_ssi)] ), 2, function(x) x-as.matrix(df_ssi[,2:ncol(df_ssi)] )[,1])
m.df_ssi <- reshape2::melt(df_ssi, id = 'datetime')
g <- ggplot(m.df_ssi) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('SSI normalized on null [J/m2]') +
  xlab('') +
  theme_minimal(); g
ggsave('output/schmidtensemble.png', g,  dpi = 300,width = 250,height = 100, units = 'mm')

df.count <- df
for (j in 2:ncol(df.count)){
  df.count[,j] <- ifelse(df.count[,j] <= 0.1, 1, 0)
}
for (j in 2:ncol(df.count)){
  df.count[,j] <- cumsum(df.count[,j])
}

df.count <- reshape2::melt(df.count, id = 'datetime')

g <- ggplot(df.count) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Cum. amount of mixed days, density diff <= 0.1 [d]') +
  xlab('') +
  theme_minimal(); g
ggsave('output/cumMixedDays.png', g,  dpi = 300,width = 150,height = 150, units = 'mm')


matrix.mixedDated <- matrix(NA, ncol = 10, nrow = length(unique(year(df$datetime))))
for (j in 2:ncol(df)){
  for (i in unique(year(df$datetime))){
    data = df[which(year(df$datetime) == i), j]
    wtr = df_wtr[which(year(df$datetime) == i), j]
    time = df[which(year(df$datetime) == i), 1]
    idx <- which(data <= 1e-2 & abs(wtr) <= 1e-1)
    matrix.mixedDated[match(i,  unique(year(df$datetime))), j-1] <- yday(time[min(idx)])
  }
}
# matrix.mixedDated <- apply(matrix.mixedDated, 2, function(x) x-matrix.mixedDated[,1])
df.mixedDated <- data.frame(matrix.mixedDated)
colnames(df.mixedDated) <- c('null','01','05','1','1.5','2','2.5','3','5','10')
df.mixedDated$year <- unique(year(df$datetime))
m.df.mixedDated <- reshape2::melt(df.mixedDated, id = 'year')

g <- ggplot(m.df.mixedDated, aes(year, value, col = variable)) +
  geom_line() + geom_point() +
  ylab('First mixing day of the year') + xlab('') +
  theme_minimal(); g
ggsave('output/firstMixingdays.png', g,  dpi = 300,width = 150,height = 150, units = 'mm')



time = rep(seq(1,12,1),5)
time = c('Jan', 'Feb', "Mar", 'Apr', "May","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",'Nov', "Dec")
scenario = ifelse(time %in% c(12,1,2,3,4), 1, 0.1)
scenario = ifelse(time %in% c('Dec',"Jan",'Feb',"Mar","Apr"), 1, 0.1)
scenario = c(1, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 1,1)
df.frame = data.frame('time' = factor(time, levels = unique(time)), 'data' = scenario)

g1 = ggplot(df.frame) +
  geom_line( aes(time, data, group =  'Scenario' )) + 
  geom_line( aes(time, data * 0, group =  'Null' ), linetype = 'dashed') + 
  geom_line( aes(time, data * 0 + 0.1, group =  'Background' ), linetype = 'dashed') + 
  geom_text(x = 2.25, y = 0.05, label = 'Null') +
  geom_text(x = 2.70, y = 0.15, label = 'Background') +
  geom_text(x = 2.5, y = 0.95, label = 'Scenario') +
  ylab('Salt load (g/kg)') +
  xlab('') +
  ylim(0,1.0) +
  theme_minimal() + 
  theme(axis.text.y =element_blank(), axis.ticks.y = element_blank()); g1
ggsave('../../../figs/Approach.png', g1,  dpi = 300,width = 200,height = 100, units = 'mm')



