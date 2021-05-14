setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)

# Load libraries for post-processing
library(lubridate)
library(gotmtools)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(colorspace)
library(patchwork)

df <- read.csv('output/density.csv')
df_wtr <- read.csv('output/wtemp.csv')
df_ssi <- read.csv('output/ssi.csv')
df_ice <- read.csv('output/ice.csv')

df$datetime <- as.POSIXct(df$datetime)
df_wtr$datetime <- as.POSIXct(df_wtr$datetime)
df_ssi$datetime <- as.POSIXct(df_ssi$datetime)
df_ice$datetime <- as.POSIXct(df_ice$datetime)

col_blues <- colorRampPalette(c("gray",'cyan', "darkblue"))

colnames(df) <- c('datetime','null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10','id')
m.df <- reshape2::melt(df, id = c('datetime', 'id'))

g_density <- ggplot(m.df) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Density differences [g/kg]') +
  xlab('') +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13))+
  ylim(0,7.5)+
  geom_hline(yintercept = 1.1, linetype = 'dashed') +
  theme_minimal()+
  theme(legend.position = 'bottom'); g_density
ggsave('figs/densitydiff.png', g_density,  dpi = 300,width = 250,height = 200, units = 'mm')

m.df_ice <- reshape2::melt(df_ice, id = c('datetime', 'id'))

g <- ggplot(subset(m.df_ice, datetime > '2013-12-30')) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Ice height [m]') +
  xlab('') +
  facet_wrap(~ id, ncol=1)+scale_color_manual(values = col_blues(13))+
  # ylim(0,7.5)+
  # geom_hline(yintercept = 1.1, linetype = 'dashed') +
  theme_minimal()+
  theme(legend.position = 'bottom'); g
ggsave('figs/ice.png', g,  dpi = 300,width = 250,height = 200, units = 'mm')

df_ssi_scaled = df_ssi
df_ssi_scaled[,2:(ncol(df_ssi)-1)] <- apply(as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] ), 2, function(x) x-as.matrix(df_ssi[,2:(ncol(df_ssi)-1)] )[,1])
m.df_ssi_scaled <- reshape2::melt(df_ssi_scaled, id = c('datetime', 'id'))
m.df_ssi <- reshape2::melt(df_ssi, id = c('datetime', 'id'))
g <- ggplot(m.df_ssi_scaled) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('SSI normalized on null [J/m2]') +
  xlab('') +
  ylim(-70,300)+
  facet_wrap(~ id, ncol =1)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom'); g
ggsave('figs/schmidtensemble_scaled.png', g,  dpi = 300,width = 250,height = 200, units = 'mm')

g <- ggplot(subset(m.df_ssi, datetime > '2013-12-30')) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('SSI normalized on null [J/m2]') +
  xlab('') +
  ylim(-70,900)+
  facet_wrap(~ id, ncol =1)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom'); g
ggsave('figs/schmidtensemble.png', g,  dpi = 300,width = 250,height = 200, units = 'mm')

df.count <- df
for (j in 2:(ncol(df.count)-1)){
  df.count[,j] <- ifelse(df.count[,j] <= 0.1, 1, 0)
}
for (j in 2:(ncol(df.count)-1)){
  idx = which(df.count$id == 'mendota')
  df.count[idx,j] <- cumsum(df.count[idx,j])
  idy = which(df.count$id == 'monona')
  df.count[idy,j] <- cumsum(df.count[idy,j])
}

df.count <- reshape2::melt(df.count, id = c('datetime', 'id'))

g <- ggplot(df.count) +
  geom_line(aes(datetime, (value), col = variable)) +
  ylab('Cum. amount of mixed days, density diff <= 0.1 [d]') +
  xlab('') +
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom'); g
ggsave('figs/cumMixedDays.png', g,  dpi = 300,width = 250,height = 150, units = 'mm')


matrix.mixedDated <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))))
matrix.mixedDated2 <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))))
for (j in 2:(ncol(df)-1)){
  for (i in unique(year(df$datetime))){
    idx = which(df$id == 'mendota')
    # data = df[which(year(df$datetime[idx]) == i), j]
    data = df[na.omit(idx[which(year(df$datetime) == i)]), j]
    # na.omit(idx[which(year(df$datetime) == i)])
    wtr = df_wtr[na.omit(idx[which(year(df$datetime) == i)]), j]
    ice = df_ice[na.omit(idx[which(year(df$datetime) == i)]), j]
    time = df[na.omit(idx[which(year(df$datetime) == i)]), 1]
    # idz <- which(data <= 1e-2 & abs(wtr) <= 1e-1)
    idz <- which(data <= 1e-1 & abs(wtr) <= 1 & ice == 0)
    matrix.mixedDated[match(i,  unique(year(df$datetime[idx]))), j-1] <- yday(time[min(idz)])
    idy = which(df$id == 'monona')
    # data = df[which(year(df$datetime) == i), j]
    data = df[na.omit(idy[which(year(df$datetime) == i)]), j]
    wtr = df_wtr[na.omit(idy[which(year(df$datetime) == i)]), j]
    ice = df_ice[na.omit(idy[which(year(df$datetime) == i)]), j]
    time = df[na.omit(idy[which(year(df$datetime) == i)]), 1]
    # idz <- which(data <= 1e-2 & abs(wtr) <= 1e-1)
    idz <- which(data <= 1e-1 & abs(wtr) <= 1 & ice == 0)
    matrix.mixedDated2[match(i,  unique(year(df$datetime[idy]))), j-1] <- yday(time[min(idz)])
  }
}
df.mixedDated2 <- data.frame(rbind(matrix.mixedDated, matrix.mixedDated2))
colnames(df.mixedDated2) <- c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10')
# matrix.mixedDated <- apply(matrix.mixedDated, 2, function(x) x-matrix.mixedDated[,1])
# matrix.mixedDated2 <- apply(matrix.mixedDated2, 2, function(x) x-matrix.mixedDated2[,1])
df.mixedDated2$year <- unique(year(df$datetime))
df.mixedDated2$id = c(rep('mendota', length(unique(year(df$datetime)))), rep('monona',length(unique(year(df$datetime)))))
df.mixedDated <- data.frame(rbind(matrix.mixedDated, matrix.mixedDated2))
colnames(df.mixedDated) <- c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10')
df.mixedDated$id = c(rep('mendota', length(unique(year(df$datetime)))), rep('monona',length(unique(year(df$datetime)))))
df.mixedDated$year <- unique(year(df$datetime))
m.df.mixedDated <- reshape2::melt(df.mixedDated, id = c('year','id'))

g_mixing <- ggplot(m.df.mixedDated, aes(year, value, col = variable)) +
  geom_line() + 
  ylab('First mixing day of the year') + xlab('') +
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom');g_mixing
ggsave('figs/firstMixingdays.png', g_mixing,  dpi = 300,width = 250,height = 150, units = 'mm')

library("scatterplot3d")
scatterplot3d(x = m.df.mixedDated$year, y= m.df.mixedDated$variable, z= m.df.mixedDated$value,
              # color = col_vector[1:n],
               type = 'p')


matrix.stratdur_mendota <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))))
matrix.stratdur_monona <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))))
for (j in 2:(ncol(df)-1)){
  for (i in unique(year(df$datetime))){
    idx = which(df$id == 'mendota')
    data = df[na.omit(idx[which(year(df$datetime) == i)]), j]
    wtr = df_wtr[na.omit(idx[which(year(df$datetime) == i)]), j]
    ice = df_ice[na.omit(idx[which(year(df$datetime) == i)]), j]
    ssi = df_ssi[na.omit(idx[which(year(df$datetime) == i)]), j]
    time = df[na.omit(idx[which(year(df$datetime) == i)]), 1]
    idz <- which(data >= 1e-1 & abs(wtr) >= 1 & ice == 0 & ssi > 0)
    for (k in idz){
      if(median(data[k:(k+6)]) > 1e-1){
        min.d <- k
        break
      }
    }
    for (k in rev(idz)){
      if(median(data[(k-6):k]) > 1e-1){
        max.d <- k
        break
      }
    }
    matrix.stratdur_mendota[match(i,  unique(year(df$datetime[idx]))), j-1] <- max.d - min.d
    
    idy = which(df$id == 'monona')
    data = df[na.omit(idy[which(year(df$datetime) == i)]), j]
    wtr = df_wtr[na.omit(idy[which(year(df$datetime) == i)]), j]
    ice = df_ice[na.omit(idy[which(year(df$datetime) == i)]), j]
    ssi = df_ssi[na.omit(idy[which(year(df$datetime) == i)]), j]
    time = df[na.omit(idy[which(year(df$datetime) == i)]), 1]
    idz <- which(data >= 1e-1 & abs(wtr) >= 1 & ice == 0 & ssi > 0)
    for (k in idz){
      if(median(data[k:(k+6)]) > 1e-1){
        min.d <- k
        break
      }
    }
    for (k in rev(idz)){
      if(median(data[(k-6):k]) > 1e-1){
        max.d <- k
        break
      }
    }
    matrix.stratdur_monona[match(i,  unique(year(df$datetime[idy]))), j-1] <- max.d - min.d
  }
}
df.stratdur <- data.frame(rbind(matrix.stratdur_mendota, matrix.stratdur_monona))
colnames(df.stratdur) <- c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10')

df.stratdur$year <- unique(year(df$datetime))
df.stratdur$id = c(rep('mendota', length(unique(year(df$datetime)))), rep('monona',length(unique(year(df$datetime)))))

m.df.stratdur <- reshape2::melt(df.stratdur, id = c('year','id'))

g_summerstrat <- ggplot(m.df.stratdur, aes(year, value, col = variable)) +
  geom_line() + 
  ylab('Summer stratification duration') + xlab('') +
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom'); g_summerstrat
ggsave('figs/Summerstratdur.png', g_summerstrat,  dpi = 300,width = 250,height = 150, units = 'mm')



matrix.icedur_mendota <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))[-c(20,21)]))
matrix.icedur_monona <- matrix(NA, ncol = 13, nrow = length(unique(year(df$datetime))[-c(20,21)]))
for (j in 2:(ncol(df)-1)){
  for (i in unique(year(df$datetime))[-c(20,21)]){ # because ice covers two years
    idx = which(df$id == 'mendota')
    data = df[na.omit(idx[which(year(df$datetime) %in% c(i,i+1))]), j]
    wtr = df_wtr[na.omit(idx[which(year(df$datetime) %in% c(i,i+1))]), j]
    ice = df_ice[na.omit(idx[which(year(df$datetime) %in% c(i,i+1))]), j]
    time = df[na.omit(idx[which(year(df$datetime) %in% c(i,i+1))]), 1]
    idz <- which(ice != 0 & yday(time) > 200 & yday(time) < 600)
    matrix.icedur_mendota[match(i,  unique(year(df$datetime[idx]))), j-1] <- length(idz)
    
    idy = which(df$id == 'monona')
    data = df[na.omit(idy[which(year(df$datetime) %in% c(i,i+1))]), j]
    wtr = df_wtr[na.omit(idy[which(year(df$datetime) %in% c(i,i+1))]), j]
    ice = df_ice[na.omit(idy[which(year(df$datetime) %in% c(i,i+1))]), j]
    time = df[na.omit(idy[which(year(df$datetime) %in% c(i,i+1))]), 1]
    idz <- which(ice != 0 & yday(time) > 200 & yday(time) < 600)
    matrix.icedur_monona[match(i,  unique(year(df$datetime[idy]))), j-1] <- length(idz)
  }
}
df.icedur <- data.frame(rbind(matrix.icedur_mendota, matrix.icedur_monona))
colnames(df.icedur) <- c('null','0.1','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','10')
df.icedur$id = c(rep('mendota', length(unique(year(df$datetime))[-c(20,21)])), rep('monona',length(unique(year(df$datetime))[-c(20,21)])))
df.icedur$year <- unique(year(df$datetime))[-c(20,21)]
m.df.icedur <- reshape2::melt(df.icedur, id = c('year','id'))

g_icestrat <- ggplot(m.df.icedur, aes(year, value, col = variable)) +
  geom_line() + 
  ylab('Ice duration') + xlab('') +
  # geom_text(data = df.mixedDated2, aes(year, null, label = (null)))+
  facet_wrap(~ id)+
  scale_color_manual(values = col_blues(13))+
  theme_minimal()+
  theme(legend.position = 'bottom');g_icestrat
ggsave('figs/iceduration.png', g_icestrat,  dpi = 300,width = 250,height = 150, units = 'mm')

g <- g_density / g_mixing / g_summerstrat / g_icestrat + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') + theme_minimal() & theme(legend.position = 'bottom'); g
ggsave('figs/scenarios.png', g,  dpi = 300,width = 150,height = 250, units = 'mm')




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
ggsave('figs/Approach.png', g1,  dpi = 300,width = 200,height = 100, units = 'mm')



