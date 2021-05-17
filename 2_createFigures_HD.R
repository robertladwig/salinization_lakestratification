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
df_lakenumber <- read.custom('output/lakenumber.csv')

# Color scale
col_blues <- colorRampPalette(c("gray",'cyan', "darkblue", 'red1','red4'))

# Convert wide to long dataframes 
m.df <- reshape2::melt(df, id = c('datetime', 'id'))
m.df_ice <- reshape2::melt(df_ice, id = c('datetime', 'id'))
m.df_wtr <- reshape2::melt(df_wtr, id = c('datetime', 'id'))
m.df_ssi <- reshape2::melt(df_ssi, id = c('datetime', 'id'))
m.df.lakenumber <- reshape2::melt(df_lakenumber, id = c('datetime', 'id'))

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
l.df_ice <- df_ice %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ice')
l.df_wtr <- df_wtr %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'wtr')
l.df_ssi <- df_ssi %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ssi')
l.df_lakenumber <- df_lakenumber %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ln')

a = l.df %>% left_join(l.df_ice) %>% left_join(l.df_wtr) %>% left_join(l.df_ssi) %>% left_join(l.df_lakenumber)

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
  mutate(median6 = rollapply(density, width = 7, median, align = 'left', fill=NA)) %>% 
  filter(density >= 1e-1 & abs(wtr) >= 1 & ice == 0 & ssi > 0) %>% 
  filter(median6 > 1e-1) %>% 
  summarize(min.d = first(yday(datetime)), max.d = last(yday(datetime))) %>% 
  mutate(duration = max.d - min.d) %>% 
  select(year, id, variable = salt, value = duration)
m.df.stratdur$variable = factor(m.df.stratdur$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

#### Create dataframe for ice duration ####
m.df.icedur = a %>% mutate(year = if_else(month(datetime) >= 10, year(datetime)+1, year(datetime))) %>% 
  group_by(id, year, salt) %>% 
  filter(ice != 0) %>%
  summarise(iceduration = n()) %>% 
  select(year, id, variable = salt, value = iceduration)
  
m.df.icedur$variable = factor(m.df.icedur$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

m.df.lakenumber = a %>% mutate(year = year(datetime), month = month(datetime)) %>%
  select(datetime, id, variable = salt, value = ln, month)
m.df.lakenumber$variable = factor(m.df.lakenumber$variable, levels = c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

################################################################################################
############ PLOTTING ############
################################################################################################
theme_custom = theme_bw(base_size = 8) +
  theme(legend.position = 'bottom',
        legend.key.width = unit(0.2,'cm'), 
        legend.key.height = unit(0.2,'cm'), 
        axis.title.x = element_blank()) 

#### Density timeseries plot ####
g_density <- ggplot(m.df) +
  geom_hline(yintercept = 1.1, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab(bquote(Density~Difference ~ (g~kg^-1))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  ylim(0,7.5)+
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_density
ggsave('figs_HD/densitydiff.png', g_density,  dpi = 500, width = 165,height = 90, units = 'mm')


#### Ice Plot ####
g <- ggplot(m.df_ice %>% filter(datetime > as.Date('2013-12-30'))) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab('Ice Thickness (m)') +
  xlab('') +
  facet_wrap(~ id, ncol=1)+scale_color_manual(values = col_blues(13))+
  # ylim(0,7.5)+
  # geom_hline(yintercept = 1.1, linetype = 'dashed') +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g

ggsave('figs_HD/ice.png', g,  dpi = 500, width = 165, height = 120, units = 'mm')


#### Mixing day plot ####
g_mixing <- ggplot(m.df.mixedDated, aes(year, as.Date(value, origin = as.Date('2019-01-01')), col = variable)) +
  geom_point(size = 1, show.legend = FALSE) + 
  ylab('First mixing day of the year') + 
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  # geom_text(aes(label = variable))+
  # geom_hline(aes(yintercept = mean(as.Date(value, origin = as.Date('2019-01-01'))), col = variable, group = variable))+
  facet_wrap(~ id) +
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_y_date(labels = date_format("%b"), breaks = 'month') +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_mixing
ggsave('figs_HD/firstMixingdays.png', g_mixing, dpi = 500, width = 165,height = 90, units = 'mm')

g_mixing_hd <- ggplot(m.df.mixedDated, aes(variable,  as.Date(value, origin = as.Date('2019-01-01')), fill = variable)) +
  geom_line(aes(variable,  as.Date(value, origin = as.Date('2019-01-01')), group = year, col = variable), size = 0.2, , show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('First mixing day of the year') + 
  scale_y_date(labels = date_format("%b"), breaks = 'month') +
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_mixing_hd

#### Schdmit Scaled Plot ####
g <- ggplot(m.df_ssi_scaled) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab(bquote(SSI~normalized~on~null ~ (J~m^-2))) +
  ylim(-70,300)+
  facet_wrap(~ id, ncol = 1)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g
ggsave('figs_HD/schmidtensemble_scaled.png', g,  dpi = 500, width = 165,height = 120, units = 'mm')

# Two years 
g <- ggplot(subset(m.df_ssi, datetime > '2013-12-30')) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab(bquote(SSI~normalized~on~null ~ (J~m^-2))) +
  ylim(-70,900)+
  facet_wrap(~ id, ncol =1)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g
ggsave('figs_HD/schmidtensemble.png', g,  dpi = 500, width = 165,height = 120, units = 'mm')

#### Cumulative density differences ####
g <- ggplot(df.count) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Cum. # of mixed days, density diff <= 0.1 [d]') +
  xlab('') +
  facet_wrap(~ id) +
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g
ggsave('figs_HD/cumMixedDays.png', g, dpi = 500, width = 165, height = 90, units = 'mm')

### Summer stratification duration ####

g_summerstrat <- ggplot(m.df.stratdur, aes(year, value, col = variable)) +
  geom_point(size = 1) + 
  ylab('Summer stratification duration') + xlab('') +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_summerstrat

ggsave('figs_HD/Summerstratdur.png', g_summerstrat,  dpi = 500, width = 165, height = 90, units = 'mm')

g_summerstrat <- ggplot(m.df.stratdur, aes(variable, value, fill = variable)) +
  geom_line(aes(variable, value, group = year, col = variable), size = 0.2, , show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('Summer stratification duration') + 
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_summerstrat
ggsave('figs_HD/Summerstratdur_salt.png', g_summerstrat,  dpi = 500, width = 165, height = 90, units = 'mm')

### Ice duration plot ####

g_icestrat <- ggplot(m.df.icedur, aes(variable, value, fill = variable)) +
  geom_line(aes(variable, value, group = year, col = variable), size = 0.2, show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('Ice duration (days)') + 
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_icestrat

ggsave('figs_HD/iceduration.png', g_icestrat, dpi = 500, width = 165, height = 90, units = 'mm')

g_lakenumber <- ggplot(subset(m.df.lakenumber, month >3 & month < 6)) +
  geom_hline(yintercept = 1.0, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab('Lake Number') + 
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  ylim(0,7.5)+
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_lakenumber
ggsave('figs_HD/Lakenumber.png', g_lakenumber,  dpi = 500, width = 165, height = 90, units = 'mm')

# Patchwork
g <- g_density / g_mixing_hd / g_summerstrat / g_icestrat + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)); g

ggsave('figs_HD/scenarios.png', g,  dpi = 500, width = 165,height = 250, units = 'mm')


# Approach
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
  annotate('text', x = 2.25, y = 0.05, label = 'Null', hjust = 0) +
  annotate('text', x = 2.25, y = 0.15, label = 'Background', hjust = 0) +
  annotate('text', x = 2.25, y = 0.95, label = 'Scenario', hjust = 0) +
  ylab(bquote(Salt~load ~ (g~kg^-1))) +
  ylim(0,1.0) +
  theme_minimal(base_size = 8) + 
  theme(axis.text.y =element_blank(), axis.ticks.y = element_blank()); g1
ggsave('figs_HD/Approach.png', g1, dpi = 500, width = 165, height = 70, units = 'mm')
