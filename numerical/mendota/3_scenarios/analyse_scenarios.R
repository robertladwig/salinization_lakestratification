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

ts.sch <- lapply(out_null, function(x) {
  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})
## Reshape to data.frame
df_null <- reshape2::melt(ts.sch, id.vars = 1)
colnames(df_null)[4] <- "model"

ncdf_10 <- '5_constantsalt+10/output/ensemble_output.nc'
out_10 <- load_var(ncdf = ncdf_10, var = "temp")

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
