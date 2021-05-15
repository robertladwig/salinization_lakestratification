setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Load libraries for post-processing
library(tidyverse)
library(lubridate)
library(gotmtools)
library(ggpubr)
library(RColorBrewer)
library(colorspace)
library(patchwork)
library(scales)
library(zoo)


# Read in data 
read.custom <- function(csv){
  new.df = read_csv(csv, ) %>% 
    mutate(id = case_when(id == 'mendota' ~ 'Lake Mendota',
                          id == 'monona' ~ 'Lake Monona'))
  colnames(new.df) <- c('datetime','null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
  return(new.df)
}

df = read.custom('output/density.csv')
df_wtr <- read.custom('output/wtemp.csv')
df_ssi <- read.custom('output/ssi.csv')
df_ice <- read.custom('output/ice.csv')

# Color scale
col_blues <- colorRampPalette(c("gray",'cyan', "darkblue", 'red1','red4'))

# Convert wide to long dataframes 
m.df <- reshape2::melt(df, id = c('datetime', 'id'))
m.df_ice <- reshape2::melt(df_ice, id = c('datetime', 'id'))
m.df_wtr <- reshape2::melt(df_wtr, id = c('datetime', 'id'))
m.df_ssi <- reshape2::melt(df_ssi, id = c('datetime', 'id'))


# Create dataframe for Schdmit normalized 
df_ssi_scaled = df_ssi
df_ssi_scaled[,2:(ncol(df_ssi)-1)] <- apply(as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] ), 2, function(x) x-as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] )[,1])
m.df_ssi_scaled <- reshape2::melt(df_ssi_scaled, id = c('datetime', 'id'))

#### Create cumulative density differences ####
df.count <- df
for (j in 2:(ncol(df.count)-1)){
  df.count[,j] <- ifelse(df.count[,j] <= 0.1, 1, 0)
}
for (j in 2:(ncol(df.count)-1)){
  idx = which(df.count$id == 'Lake Mendota')
  df.count[idx,j] <- cumsum(df.count[idx,j])
  idy = which(df.count$id == 'Lake Monona')
  df.count[idy,j] <- cumsum(df.count[idy,j])
}

df.count <- reshape2::melt(df.count, id = c('datetime', 'id'))

#### Create dataframe for mixing date ####
# Convert wide to long dataframes 
l.df <- df %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'density')
colnames(df_ice) <- c('datetime','null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
l.df_ice <- df_ice %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ice')
colnames(df_wtr) <- c('datetime','null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
l.df_wtr <- df_wtr %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'wtr')
colnames(df_ssi) <- c('datetime','null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
l.df_ssi <- df_ssi %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ssi')

a = l.df %>% left_join(l.df_ice) %>% left_join(l.df_wtr) %>% left_join(l.df_ssi)

m.df.mixedDated = a %>% group_by(id, year = year(datetime), salt) %>% 
  filter(density <= 1e-1 & abs(wtr) <= 1 & ice == 0) %>% 
  summarize(yday = first(yday(datetime))) %>% 
  select(year, id, variable = salt, value = yday)
m.df.mixedDated$variable = factor(m.df.mixedDated$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))


library("scatterplot3d")
scatterplot3d(x = m.df.mixedDated$year, y= m.df.mixedDated$variable, z= m.df.mixedDated$value,
              # color = col_vector[1:n],
               type = 'p')

#### Create dataframe for stratfication duration ####
m.df.stratdur = a %>% group_by(id, year = year(datetime), salt) %>% 
  arrange(year, id, salt) %>% 
  mutate(median6 = rollapply(density, width = 7, median, align = 'left', fill=NA)) %>% 
  filter(density >= 1e-1 & abs(wtr) >= 1 & ice == 0 & ssi > 0) %>% 
  filter(median6 > 1e-1) %>% 
  summarize(min.d = first(yday(datetime)), max.d = last(yday(datetime))) %>% 
  mutate(duration = max.d - min.d) %>% 
  select(year, id, variable = salt, value = duration)
m.df.stratdur$variable = factor(b$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

#### Create dataframe for ice duration ####
m.df.icedur = a %>% mutate(year = if_else(month(datetime) >= 10, year(datetime)+1, year(datetime))) %>% 
  group_by(id, year, salt) %>% 
  arrange(year, id, salt) %>% 
  filter(ice != 0) %>%
  summarise(iceduration = n()) %>% 
  select(year, id, variable = salt, value = iceduration)
  
m.df.icedur$variable = factor(m.df.icedur$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))




