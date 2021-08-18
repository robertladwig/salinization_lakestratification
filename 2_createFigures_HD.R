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
  colnames(new.df) <- c('datetime','null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
  return(new.df)
}

df = read.custom('output/density.csv')
df_wtr <- read.custom('output/wtemp.csv')
df_salt <- read.custom('output/salt.csv')
df_ssi <- read.custom('output/ssi.csv')
df_ice <- read.custom('output/ice.csv')
df_lakenumber <- read.custom('output/lakenumber.csv')

# Color scale
col_blues <- colorRampPalette(c("gray",'cyan', "darkblue", 'red1','red4'))

# Convert wide to long dataframes 
m.df <- reshape2::melt(df, id = c('datetime', 'id'))
m.df_ice <- reshape2::melt(df_ice, id = c('datetime', 'id'))
m.df_wtr <- reshape2::melt(df_wtr, id = c('datetime', 'id'))
m.df_salt <- reshape2::melt(df_salt, id = c('datetime', 'id'))
m.df_ssi <- reshape2::melt(df_ssi, id = c('datetime', 'id'))
m.df.lakenumber <- reshape2::melt(df_lakenumber, id = c('datetime', 'id'))

# Create dataframe for ice normalized 
df_ice_scaled = df_ice
df_ice_scaled[,2:(ncol(df_ice)-1)] <- apply(as.matrix(df_ice[,2:(ncol(df_ice)-1)] ), 2, function(x) x-as.matrix(df_ice[,2:(ncol(df_ice)-1)] )[,1])
m.df_ice_scaled <- reshape2::melt(df_ice_scaled, id = c('datetime', 'id'))

# Create dataframe for Schdmit normalized 
df_ssi_scaled = df_ssi
df_ssi_scaled[,2:(ncol(df_ssi)-1)] <- apply(as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] ), 2, function(x) x-as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] )[,1])
m.df_ssi_scaled <- reshape2::melt(df_ssi_scaled, id = c('datetime', 'id'))

# Create dataframe for Lakenumber normalized
df_lakenumber_scaled = df_lakenumber
df_lakenumber_scaled[,2:(ncol(df_lakenumber)-1)] <- apply(as.matrix(df_lakenumber[,2:(ncol(df_lakenumber)-1)] ), 2, function(x) x-as.matrix(df_lakenumber[,2:(ncol(df_lakenumber)-1)] )[,1])
m.df_lakenumber_scaled <- reshape2::melt(df_lakenumber_scaled, id = c('datetime', 'id'))

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
l.df_lakenumber_scaled <- df_lakenumber_scaled %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ln_scaled')
l.df_ice_scaled <- df_ice_scaled %>% pivot_longer(cols = -c(datetime,id), names_to = 'salt', values_to = 'ice_scaled')


a = l.df %>% left_join(l.df_ice) %>% left_join(l.df_wtr) %>% left_join(l.df_ssi) %>% left_join(l.df_lakenumber) %>% left_join(l.df_lakenumber_scaled) %>%
  left_join(l.df_ice_scaled)

m.df.mixedDated = a %>% mutate(ice_movavg = zoo::rollmean(ice, k = 90, fill = NA)) %>%
  group_by(id, year = year(datetime), salt) %>% 
  filter(density <= 1e-1 & abs(wtr) <= 1 & ice_movavg <= 1e-5) %>% 
  summarize(yday = first(yday(datetime))) %>% 
  select(year, id, variable = salt, value = yday)
m.df.mixedDated$variable = factor(m.df.mixedDated$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

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
m.df.stratdur$variable = factor(m.df.stratdur$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

m.df.stratdur_scaled = m.df.stratdur %>% left_join(m.df.stratdur %>% filter(variable == 'null') %>% select(-variable) %>% rename(null = value)) %>% 
  mutate(value = value - null) %>% 
  select(-null)

m.df.stratdur_scaled$variable = factor(m.df.stratdur_scaled$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

#### Create dataframe for ice duration ####
m.df.icedur = a %>% mutate(year = if_else(month(datetime) >= 10, year(datetime)+1, year(datetime))) %>% 
  group_by(id, year, salt) %>% 
  filter(ice != 0) %>%
  summarise(iceduration = n()) %>% 
  select(year, id, variable = salt, value = iceduration)

m.df.icedur$variable = factor(m.df.icedur$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

m.df.icedur %>% filter(variable == 'null')

m.df.icedur_scaled = m.df.icedur %>% left_join(m.df.icedur %>% filter(variable == 'null') %>% select(-variable) %>% rename(null = value)) %>% 
  mutate(value = value - null) %>% 
  select(-null)

m.df.icedur_scaled$variable = factor(m.df.icedur_scaled$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))


m.df.lakenumber = a %>% mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  select(datetime, id, variable = salt, value = ln, month, day)
m.df.lakenumber$variable = factor(m.df.lakenumber$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

m.df_lakenumber_scaled = a %>% mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  select(datetime, id, variable = salt, value = ln_scaled, month, day)
m.df_lakenumber_scaled$variable = factor(m.df_lakenumber_scaled$variable, levels = c('null','BG','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10'))

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
  ylab(bquote(Density~Difference ~ (kg~m^-3))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  ylim(0,7.5)+
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_density
ggsave('figs_HD/densitydiff.png', g_density,  dpi = 500, width = 165,height = 90, units = 'mm')

g_density_08 <- ggplot(m.df) +
  geom_hline(yintercept = 1.1, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab(bquote(Density~Difference ~ (kg~m^-3))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  ylim(0,10) +
  xlim(as.Date('2008-01-01'), as.Date('2010-07-01')) +
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_density_08

#### Ice Plot ####
g <- ggplot(m.df_ice ) +#%>% filter(datetime > as.Date('2013-12-30'))) +
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
  geom_line(aes(variable,  as.Date(value, origin = as.Date('2019-01-01')), group = year, col = variable), size = 0.2, show.legend = FALSE) +
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
  geom_line(aes(variable, value, group = year, col = variable), size = 0.2, show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('Summer stratification duration') + 
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_summerstrat
ggsave('figs_HD/Summerstratdur_salt.png', g_summerstrat,  dpi = 500, width = 165, height = 90, units = 'mm')


g_summerstrat_scaled <- ggplot(m.df.stratdur_scaled, aes(variable, value, fill = variable)) +
  geom_line(aes(variable, value, group = year, col = variable), size = 0.2, show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('Summer stratification duration normalized on null (days)') + 
  ylab(expression(atop("Summer stratification duration", paste("normalized on null (days)")))) +
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_summerstrat_scaled
ggsave('figs_HD/Summerstratdur_salt_normalized.png', g_summerstrat_scaled,  dpi = 500, width = 165, height = 90, units = 'mm')

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


g_icestrat_scaled <- ggplot(m.df.icedur_scaled, aes(variable, value, fill = variable)) +
  geom_line(aes(variable, value, group = year, col = variable), size = 0.2, show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  ylab('Ice duration normalized on null (days)') + 
  ylab(expression(atop("Ice duration", paste("normalized on null (days)")))) +
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_icestrat_scaled

ggsave('figs_HD/iceduration_norm.png', g_icestrat_scaled, dpi = 500, width = 165, height = 90, units = 'mm')

g_lakenumber <- ggplot(subset(m.df.lakenumber, month >=3 & month < 6& day %in% c(1,15,30))) +
  geom_hline(yintercept = 1.0, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), col = variable), size = 0.2) +
  ylab('Lake Number') + 
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  ylim(0,7.5)+
  theme_custom +
  guides(color=guide_legend(nrow=2,byrow=TRUE)); g_lakenumber
ggsave('figs_HD/Lakenumber.png', g_lakenumber,  dpi = 500, width = 165, height = 90, units = 'mm')

g_lakenumber <- ggplot(subset(m.df_lakenumber_scaled, month >=3 & month < 6 ),#& day %in% c(1,15,30)),
                       aes(variable, value, fill = variable)) +
  geom_line(aes(variable, value, group = datetime, col = variable), size = 0.2, show.legend = FALSE) +
  geom_point(size = 1.2, shape = 21, stroke = 0.1, alpha = 0.8, show.legend = FALSE) + 
  # ylab('Spring Lake Number normalized on null') + 
  ylab(expression(atop("Lake Number (March-May)", paste("normalized on null (-)")))) +
  xlab(bquote(Salt~Scenario ~ (g~kg^-1))) +
  # geom_hline(yintercept = 1.0, linetype = 'dashed', size = 0.2) +
  facet_wrap(~ id)+
  ylim(0,2)+
  scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  scale_fill_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = 'none'); g_lakenumber
ggsave('figs_HD/Lakenumber_salt.png', g_lakenumber,  dpi = 500, width = 165, height = 90, units = 'mm')

# Patchwork
g <- g_density / g_icestrat_scaled / g_mixing_hd  / g_lakenumber / g_summerstrat_scaled + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')',
                  title = 'Ensemble') & 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)); g

ggsave('figs_HD/scenarios_Ensemble.png', g,  dpi = 500, width = 165,height = 250, units = 'mm')


# Approach
time = rep(seq(1,12,1),5)
time = c('Jan', 'Feb', "Mar", 'Apr', "May","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",'Nov', "Dec")
scenario = ifelse(time %in% c(12,1,2,3,4), 1, 0.1)
scenario = ifelse(time %in% c('Dec',"Jan",'Feb',"Mar","Apr"), 1, 0.1)
scenario = c(1, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 1,1)
df.frame = data.frame('time' = factor(time, levels = unique(time)), 'data' = scenario)

g1 = ggplot(df.frame) +
  geom_line( aes(time, data, group =  'Scenario'), col = 'red4') + 
  geom_line( aes(time, data * 0, group =  'Null'), linetype = 'dashed') + 
  geom_line( aes(time, data * 0 + 0.1, group =  'Background' ), linetype = 2, col = 'lightblue4') + 
  annotate('text', x = 1, y = 0.05, label = 'Null', hjust = 0, size = 2.5) +
  annotate('text', x = 1, y = 0.15, label = 'Background', hjust = 0, size = 2.5, col = 'lightblue4') +
  annotate('text', x = 1, y = 0.95, label = 'Scenario', hjust = 0, size = 2.5, col = 'red4') +
  ylab(bquote(Salt~load ~ (g~kg^-1))) +
  ylim(0,1.0) +
  theme_minimal(base_size = 8) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45),
        axis.text.y =element_blank(),
        axis.ticks.y = element_blank()); g1
# ggsave('figs_HD/Approach.png', g1, dpi = 500, width = 165, height = 70, units = 'mm')
ggsave('figs_HD/Approach2.pdf', g1, dpi = 500, width = 2.5, height = 1.5, units = 'in')


## some comparison plots
ncdf <- 'numerical/mendota/3_scenarios/1_null/output/ensemble_output.nc'
model = c('GLM', 'GOTM', 'Simstrat')
p1 <- plot_ensemble(ncdf, model = model,
                    var = "temp", depth = 2,
                    residuals = TRUE) 
p2 <- plot_ensemble(ncdf, model = model,
                    var = "temp", depth = 22,
                    residuals = TRUE)
# Arrange the two plots above each other
p <- p1[[1]] / p2[[1]] + #ggarrange(p1[[1]] + theme_light(),
  #p2[[1]] + theme_light(), ncol = 1, nrow = 2) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')  & 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) & theme_minimal(); p
ggsave('figs_HD/temp_ensemble.png', p,  dpi = 500, width = 250,height = 165, units = 'mm')

ncdf <- 'numerical/mendota/3_scenarios/7_constantsalt+1/output/ensemble_output.nc'
model = c('GLM', 'GOTM', 'Simstrat')
p1 <- plot_ensemble(ncdf, model = model,
                    var = "salt", depth = 2,
                    residuals = TRUE) + xlim(as.POSIXct('2010-01-01'), as.POSIXct('2015-12-31'))
p2 <- plot_ensemble(ncdf, model = model,
                    var = "salt", depth = 22,
                    residuals = TRUE) + xlim(as.POSIXct('2010-01-01'), as.POSIXct('2015-12-31'))
# Arrange the two plots above each other
p <- p1 / p2+ #ggarrange(p1[[1]] + theme_light(),
  #p2[[1]] + theme_light(), ncol = 1, nrow = 2) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')  & 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) & theme_minimal(); p
ggsave('figs_HD/salt_ensemble.png', p,  dpi = 500, width = 250,height = 165, units = 'mm')


# residence times
inflow.mendota <- read_csv('numerical/mendota/1_calibration/LakeEnsemblR_inflow_standard.csv')
mean.mendota <- mean(inflow.mendota$Flow_metersCubedPerSecond)

hypso.mendota <- read.csv('numerical/mendota/1_calibration/LakeEnsemblR_bathymetry_standard.csv')
volume.mendota <- pracma::trapz(hypso.mendota$Depth_meter, hypso.mendota$Area_meterSquared)

print(paste0('residence time of Mendota is ', round(volume.mendota/mean.mendota/86400/365.25,2), ' years'))

inflow.monona <- read.csv('numerical/monona/1_calibration/LakeEnsemblR_inflow_standard.csv')
mean.monona <- mean(inflow.monona$Flow_metersCubedPerSecond)

hypso.monona <- read.csv('numerical/monona/1_calibration/LakeEnsemblR_bathymetry_standard.csv')
volume.monona <- pracma::trapz(hypso.monona$Depth_meter, hypso.monona$Area_meterSquared)

print(paste0('residence time of Mendota is ', round(volume.monona/mean.monona/86400/365.25,2), ' years'))

# ggsave('output/ensemble_ts2m.png', p,  dpi = 300,width = 384,height = 180, units = 'mm')

### field monitoring data by linnea ####
me_epi_1920 <- readRDS(file = 'fieldmonitoring/ME_EPI_2019-20.rds') %>% mutate('')
me_hyp_1920 <- readRDS(file = 'fieldmonitoring/ME_HYPO_2019-20.rds')

mo_epi_1920 <- readRDS(file = 'fieldmonitoring/MO_EPI_2019-20.rds')
mo_hyp_1920 <- readRDS(file = 'fieldmonitoring/MO_HYPO_2019-20.rds')

mo_epi_2021 <- readRDS(file = 'fieldmonitoring/MO_EPI_2020-21.rds')
mo_hyp_2021 <- readRDS(file = 'fieldmonitoring/MO_HYPO_2020-21.rds')

MSNweather = read_csv('data/USWOOO14837_DaneCountyAirport.csv')

df_me <- merge(me_epi_1920, me_hyp_1920, by = 'date') %>%
  mutate('tempgrad' = Temp.y - Temp.x,
         'temp_2' = Temp.x,
         'temp_23.5' = Temp.y,
         'cond_2' = runningmean.x,
         'cond_23.5' = runningmean.y,
         'cl_2' = runningmean.x * 0.55/1000,
         'cl_23.5' = runningmean.y * 0.55/1000) %>%
  select(date, tempgrad, temp_2, temp_23.5, cond_2, cond_23.5, cl_2, cl_23.5)

df_mo19 <- merge(mo_epi_1920, mo_hyp_1920, by = 'date') %>%
  mutate('tempgrad' = Temp.y - Temp.x,
         'temp_2' = Temp.x,
         'temp_23.5' = Temp.y,
         'cond_2' = runningmean.x,
         'cond_23.5' = runningmean.y,
         'cl_2' = runningmean.x * 0.55/1000,
         'cl_23.5' = runningmean.y * 0.55/1000) %>%
  select(date, tempgrad, temp_2, temp_23.5, cond_2, cond_23.5, cl_2, cl_23.5)

df_mo20 <- merge(mo_epi_2021, mo_hyp_2021, by = 'date') %>%
  mutate('tempgrad' = Temp.y - Temp.x,
         'temp_2' = Temp.x,
         'temp_23.5' = Temp.y,
         'cond_2' = runningmean.x,
         'cond_23.5' = runningmean.y,
         'cl_2' = runningmean.x * 0.55/1000,
         'cl_23.5' = runningmean.y * 0.55/1000) %>%
  select(date, tempgrad, temp_2, temp_23.5, cond_2, cond_23.5, cl_2, cl_23.5)


# https://www.aos.wisc.edu/~sco/lakes/msnicesum.html
plotEC <- function(df, usetitle, icedate, icedate2){
  df2 = df %>% 
    mutate(EC.diff = (cond_23.5 - cond_2)/80) %>% 
    select(date, temp_2, temp_23.5, EC.diff) %>% 
    pivot_longer(cols = temp_2:EC.diff, names_to = 'var')
  ggplot(df2) +
    geom_line(aes(x = date, y = value, group = var, col = var), size = 0.5) +
    scale_color_manual(values = c('red1','lightblue2','lightblue4'), labels = c('\u0394 EC', 'Surface Temp', 'Bottom Temp')) +
    geom_vline(xintercept = as.POSIXct(icedate), linetype = 'dashed', size = 0.2) +
    geom_vline(xintercept = as.POSIXct(icedate2), linetype = 'dashed', size = 0.2) +
    ylab('Temp (°C)') +
    # labs(title = usetitle) +
    scale_y_continuous(sec.axis = sec_axis(~.*80, name = bquote('\u0394'~EC ~ (µS~cm^-1)))) +
    theme_custom +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 6),
          legend.margin=unit(-0.6,"cm"),
          plot.title = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.title.y.right = element_text(color = 'red1'),
          axis.title.y.left = element_text(color = 'lightblue4')) 
}
g1 = plotEC(df_me, usetitle = 'Mendota 2019-2020', icedate = '2020-03-22', icedate2 = '2020-01-12') +
  theme(legend.position = c(0.18, 0.75)); g1
g2 = plotEC(df_mo19, usetitle = 'Monona 2019-2020', icedate = '2020-03-20', icedate2 = '2020-01-12') +
  theme(legend.position = 'none')
g3 = plotEC(df_mo20, usetitle = 'Monona 2020-2021', icedate = '2021-03-22', icedate2 = '2020-12-29') +
  theme(legend.position = 'none')

plotWeather <- function(df, icedate, usetitle) {
  ggplot(MSNweather %>% filter(DATE >= as.Date(icedate))) +
    geom_col(aes(x = DATE, y = AWND)) +
    geom_col(data = MSNweather, aes(x = DATE, y = AWND), alpha = 0.2) +
    scale_y_continuous(breaks = c(0,4,8), expand = c(0,0.2)) +
    scale_x_date(limits = as.Date(c(df$date[1], df$date[nrow(df)]))) +
    labs(title = usetitle) +
    theme_custom +
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 7),
          axis.title.x = element_blank())
}
w1 = plotWeather(df = df_me, icedate = '2020-03-22', usetitle = 'A) Mendota 2019-2020')
w2 = plotWeather(df = df_mo19, icedate = '2020-03-20', usetitle = 'B) Monona 2019-2020')
w3 = plotWeather(df = df_mo20, icedate = '2021-03-22', usetitle = 'C) Monona 2020-2021')

w1 / g1 / w2 / g2 / w3/ g3 + plot_layout(heights = c(1,3,1,3,1,3))

ggsave('figs_HD/fieldmonitoring_model2.png', dpi = 700, width = 3.25,height = 5, units = 'in')


### Other plots 
g4 <- ggplot(m.df_wtr %>% filter(id == 'Lake Mendota' & variable == 2)) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), color = '\u0394 Temp'), size = 0.4) +
  ylab('\u0394 Temp [°C]') +
  # ylim(0,15) +
  xlab('2014-2015') +
  labs(title = 'Lake Mendota Simulation') +
  geom_line(data = m.df_salt %>% filter(id == 'Lake Mendota' & variable == 2), 
            aes(datetime, y = (value)*10, col = '\u0394 Salinity/EC'), size = 0.4) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = bquote('\u0394'~Salinity ~ (g~kg^-1)))) +
  scale_x_date(labels = date_format("%b"), breaks = '2 months', limits = c(as.Date('2014-01-01'), as.Date('2014-05-31'))) +
  scale_colour_manual(values = c("red4", "lightblue4")) +
  theme_custom + 
  theme(legend.position = c(0.8, 0.95),
        legend.title = element_blank(), 
        axis.title.y.right = element_text(color = 'red4'),
        axis.title.y.left = element_text(color = 'lightblue4')) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)); g4


g5 <- ggplot(m.df_wtr %>% filter(id == 'Lake Monona' & variable == 2)) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  geom_line(aes(datetime, (value), color = '\u0394 Temp'), size = 0.4) +
  ylab('\u0394 Temp [°C]') +
  # ylim(0,15) +
  xlab('2014-2015') +
  labs(title = 'Lake Monona Simulation') +
  geom_line(data = m.df_salt %>% filter(id == 'Lake Monona' & variable == 2), 
            aes(datetime, y = (value)*10, col = '\u0394 Salinity/EC'), size = 0.4) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = bquote('\u0394'~Salinity ~ (g~kg^-1)))) +
  scale_x_date(labels = date_format("%b"), breaks = '2 months', limits = c(as.Date('2014-01-01'), as.Date('2014-05-31'))) +
  scale_colour_manual(values = c("red4", "lightblue4")) +
  theme_custom + 
  theme(legend.position = c(0.8, 0.95),
        legend.title = element_blank(), 
        axis.title.y.right = element_text(color = 'red4'),
        axis.title.y.left = element_text(color = 'lightblue4')) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)); g5

# layout <- "
# AACC
# AACC
# AACC
# ##DD
# BBDD
# BBEE
# FFEE
# "
# 
# p = g4 + g5 + g1 + g2 + g3 + guide_area() +
#   plot_layout(design = layout, guides = 'collect') +
#   plot_annotation(tag_levels = 'A', tag_suffix = ')')  &  
#   theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) ; p
# ggsave('figs_HD/fieldmonitoring_model.png', p,  dpi = 500, width = 6.5,height = 4, units = 'in')
# 

g1 <- ggplot(df_me) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cl_2*3, colour = "PSU 2m")) +
  geom_line(aes(date, y = cl_23.5*3, colour = "PSU 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "PSU [g/kg]")) +
  xlab('') +
  ggtitle('Mendota 2019-2020')+
  theme_minimal();g1

g2 <- ggplot(df_mo19) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cl_2*3, colour = "PSU 2m")) +
  geom_line(aes(date, y = cl_23.5*3, colour = "PSU 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "PSU [g/kg]")) +
  xlab('') +
  ggtitle('Monona 2019-2020')+
  theme_minimal();g2

g3 <- ggplot(df_mo20) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cl_2*3, colour = "PSU 2m")) +
  geom_line(aes(date, y = cl_23.5*3, colour = "PSU 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "PSU [g/kg]")) +
  xlab('') +
  ggtitle('Monona 2020-2021')+
  theme_minimal();g3

p = g1 + g2 + g3 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')  & theme_minimal()& 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) ; p
ggsave('figs_HD/fieldmonitoring_psu.png', p,  dpi = 500, width = 250,height = 130, units = 'mm')



g1 <- ggplot(df_me) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cond_2/500, colour = "Cl 2m")) +
  geom_line(aes(date, y = cond_23.5/500, colour = "Cl 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "EC [uS/cm]")) +
  xlab('') +
  ggtitle('Mendota 2019-2020')+
  theme_minimal();g1

g2 <- ggplot(df_mo19) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cond_2/500, colour = "Cl 2m")) +
  geom_line(aes(date, y = cond_23.5/500, colour = "Cl 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "EC [uS/cm]")) +
  xlab('') +
  ggtitle('Monona 2019-2020')+
  theme_minimal();g2

g3 <- ggplot(df_mo20) +
  geom_line( aes(date, abs(tempgrad), col = 'Delta T')) +
  ylab('Wtemp density [deg C/m]') +
  geom_line(aes(date, y = cond_2/500, colour = "Cl 2m")) +
  geom_line(aes(date, y = cond_23.5/500, colour = "Cl 23.5m")) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "EC [uS/cm]")) +
  xlab('') +
  ggtitle('Monona 2020-2021')+
  theme_minimal();g3

p = g1 + g2 + g3 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')  & theme_minimal()& 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) ; p
ggsave('figs_HD/fieldmonitoring_ec.png', p,  dpi = 500, width = 250,height = 130, units = 'mm')


# https://www.aos.wisc.edu/~sco/lakes/msnicesum.html
g1 <- ggplot(df_me%>% mutate('title' = 'Mendota 2019-2020')) +
  geom_line( aes(date, abs(tempgrad), col = '\u0394T')) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  geom_vline(xintercept = as.POSIXct('2020-03-22'), linetype = 'dashed', size = 0.2) +
  ylab('\u0394Temp. [K]') +
  geom_line(aes(date, y = cl_23.5*3 - cl_2*3, colour = "\u0394S")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "\u0394Salt [g/kg]")) +
  xlab('') +
  facet_grid(. ~ title) +
  theme_custom;g1

g2 <- ggplot(df_mo19%>% mutate('title' = 'Monona 2010-2020')) +
  geom_line( aes(date, abs(tempgrad), col = '\u0394T')) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  geom_vline(xintercept = as.POSIXct('2020-03-20'), linetype = 'dashed', size = 0.2) +
  ylab('\u0394Temp. [K]') +
  geom_line(aes(date, y = cl_23.5*3 - cl_2*3, colour = "\u0394S")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "\u0394Salt [g/kg]")) +
  xlab('') +
  facet_grid(. ~ title) +
  theme_custom;g2

g3 <- ggplot(df_mo20 %>% mutate('title' = 'Monona 2020-2021')) +
  geom_line( aes(date, abs(tempgrad), col = '\u0394T')) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  geom_vline(xintercept = as.POSIXct('2021-03-22'), linetype = 'dashed', size = 0.2) +
  ylab('\u0394Temp. [K]') +
  geom_line(aes(date, y = cl_23.5*3 - cl_2*3, colour = "\u0394S")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "\u0394Salt [g/kg]")) +
  xlab('') +
  facet_grid(. ~ title) +
  theme_custom;g3

g4 <- ggplot(subset(m.df_wtr, variable == '2')) +
  geom_line(aes(datetime, abs(value), color = '\u0394T'), size = 1) +
  # geom_line(aes(datetime, (value), color = '\u0394S'), size = 1) +
  ylab('\u0394Temp. [K]') +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 0.2) +
  # ylab(bquote(Salt ~ (g~kg^-1))) +
  ylim(0,15)+
  xlim(as.Date('2014-01-01'), as.Date('2015-12-31')) +
  geom_line(data = subset(m.df_salt, variable == '2'), aes(datetime, y = (value)*10,
                                                           col = '\u0394S'), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "\u0394Salt [g/kg]")) +
  facet_wrap(~ id, ncol=1)+
  # scale_color_manual(values = col_blues(13), name = bquote(Salt~Scenario ~ (g~kg^-1))) +
  theme_custom + 
  scale_colour_manual(values = c("blue", "red")) +
  theme(legend.position = c(0.8, 0.95)) +
  labs(color = "") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)); g4

p = g4 + (g1  / g2 / g3 ) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')  & theme_minimal()& 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) ; p
ggsave('figs_HD/fieldmonitoring_model.png', p,  dpi = 500, width = 250,height = 250, units = 'mm')
