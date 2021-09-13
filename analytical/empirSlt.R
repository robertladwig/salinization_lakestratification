library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(rLakeAnalyzer)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# get density from temp and salt
get_dens <- function(temp, salt){
  dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
    (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
    (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
       5.3875 * 10^-9* temp^4) * salt+
    (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
    (4.8314*  10^-4 ) * salt
  return(dens)
}

name_lake ='monona'
bath.data <- read_csv('monona_bathymetry.csv')

# optimization function
optm_salt <- function(x, wtr.dat = 12, wsp.dat = 1, bath.data = bath.data){
  temp_top = wtr.dat
  temp = temp_top  # deg C
  salt_top = 0
  salt = x # g/kg
  wnd = wsp.dat
  
  
  bth <- bath.data$Depth_meter # seq(from = 0, to = 25, by = 1)
  area <- bath.data$Area_meterSquared # seq(from = 39850000, to = 0, length.out = length(bth))

  zv <- bth %*% area / sum(area)
  wtr <- c(rep(temp_top, round(zv)), rep(temp, length(bth) - round(zv)))
  slt <- c(rep(get_dens(temp_top, salt_top), round(zv)), rep(get_dens(temp,salt), length(bth) - round(zv)))
  
  ssi = schmidt.stability(wtr = wtr, depths = bth, bthA = area, bthD = bth, sal = slt)
  
  wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd^2
  # kinetic <- get_dens(temp = temp_top, salt = salt_top) *
  #   wnd_shear *
  #   area[1]^(3/2) *
  #   (1 - (max(bth)-zv)/max(bth))
  # stable <- ssi * area[1] *
  #   (1 - (max(bth) - zv)/max(bth))#15

  kinetic <- get_dens(temp = temp, salt = salt) *
    wnd_shear *
    area[1]^(3/2) *
    (max(bth) - zv)
  stable <- ssi * area[1] *
  (max(bth) - zv)
  
  bthA = area
  bthD = bth
  uStar = sqrt(wnd_shear)
  St = ssi
  metaT = zv
  metaB = 15
  averageHypoDense = get_dens(temp = temp, salt = salt) 
  g	<-	9.81
  dz	<-	0.1
  # if bathymetry has negative values, remove.
  # intepolate area and depth to 0
  Ao	<-	bthA[1]
  Zo	<-	bthD[1]
  if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
  #interpolates the bathymetry data
  layerD	<-	seq(Zo,max(bthD),dz)
  layerA	<-	stats::approx(bthD,bthA,layerD)$y
  #find depth to the center of volume
  Zv = layerD*layerA*dz                    
  Zcv = sum(Zv)/sum(layerA)/dz

  St_uC = St*Ao/g
  # Calculates the Lake Number according to the formula provided
  Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
  
  # print(abs(kinetic - stable))
  # print(abs( g*St_uC*(metaT+metaB) -
               # (2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv) ))
  # return(abs(kinetic - stable))
  return(abs( g*St_uC*(metaT+metaB) -
                (2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv) ))
}
plot_salt <- function(x, wtr.dat = 12, wsp.dat = 1, bath.data = bath.data){
  temp_top = wtr.dat
  temp = temp_top  # deg C
  salt_top = 0
  salt = x # g/kg
  wnd = wsp.dat
  

  bth <- bath.data$Depth_meter # seq(from = 0, to = 25, by = 1)
  area <- bath.data$Area_meterSquared # seq(from = 39850000, to = 0, length.out = length(bth))
  zv <- bth %*% area / sum(area)
  wtr <- c(rep(temp_top, round(zv)), rep(temp, length(bth) - round(zv)))
  slt <- c(rep(get_dens(temp_top, salt_top), round(zv)), rep(get_dens(temp,salt), length(bth) - round(zv)))
  
  
  ssi = schmidt.stability(wtr = wtr, depths = bth, bthA = area, bthD = bth, sal = slt)
  
  wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd^2

  kinetic <- get_dens(temp = temp, salt = salt) *
    wnd_shear *
    area[1]^(3/2) *
    (max(bth) - zv)
  stable <- ssi * area[1] *
    (max(bth) - zv)

  # return(c(x, kinetic, stable))
  
  bthA = area
  bthD = bth
  uStar = sqrt(wnd_shear)
  St = ssi
  metaT = zv
  metaB = 15
  averageHypoDense = get_dens(temp = temp, salt = salt) 
  g	<-	9.81
  dz	<-	0.1
  # if bathymetry has negative values, remove.
  # intepolate area and depth to 0
  Ao	<-	bthA[1]
  Zo	<-	bthD[1]
  if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
  #interpolates the bathymetry data
  layerD	<-	seq(Zo,max(bthD),dz)
  layerA	<-	stats::approx(bthD,bthA,layerD)$y
  #find depth to the center of volume
  Zv = layerD*layerA*dz                    
  Zcv = sum(Zv)/sum(layerA)/dz
  
  St_uC = St*Ao/g
  # Calculates the Lake Number according to the formula provided
  Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
  
  return(c(x,  (2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv) ,g*St_uC*(metaT+metaB)  ))
}

# https://watermark.silverchair.com/20-10-1927.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAs4wggLKBgkqhkiG9w0BBwagggK7MIICtwIBADCCArAGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMLx4nUnGRfFHcce9AAgEQgIICgTyM3V2P57QKYMWOJ1v5BZcBjRpbjfkIxaqOoxqKpc-x6WAqu1xBubzaBhTxcmIlN-qK1tVElip6ZnbwM05_RVBYN_rvAuOfpT9MrQcujq7umh70w8AnD9h1LwgUApxONTLQ0T_tFXq2oNbFOgNa1TKCOq4ExKBVe47weRFPVRWaqn3i2fZi5iUR1Han1gxk6DRaLNCjG09L5zDjP6aFf6i4jTiMDjMncm-rcGv7B50cirGW7O9XcC83mH4_64eFVo4VB2B6mGE48st-_vVTJ-KJFURdAlTGQ5J7_p3woowFLO_vA6FZmNmQ8MkMwSoDO9Eghq3WTyj8IasfGM5uQ9361EvIMQ0xb_ql2gLbKsKEB-miD7GDt6uzdyUuDwjbaVcgkk1LI-ssq-ffk4lX6yE31--srLLx0G9Ule4f_udXCbtW06JBD4H85kaIRTMepKUpHKI2pJffNwzmCWmTYfsIl0616IEjMvHE6FyfxnIhYnLDUGt6_6cfTp8iIIqRMofAZvDpkFp1WUEaojarZHmTd1rVRSniYsmTkSmUwBit8ybUKnjecbPhAjxpaZjV_e7Bf1eHlIe18QGqwJM9Svwh5sQ0Hu3L03scIrA-fuUSXh9-jbB9O0oBpJ4v-877PLad2OFhES2C6w-2NFlQHds_pooIy6YeeFuD7P1YLSPsRChxwx4sbX1mmM2uOdEPgnA0PNj8fg_oQ8-GTPnAweAY-rZAVubJnJT9RlUYmz7SAHFUsAWQSgp6SgKyJo0wzDkgdOdVqRRuOxMNDLqFza2AqhC4pITUWf4ZqHsVziM2ABcszeASjaGaA_1JhCFzpFYefu1RYTDgjxjsIlYqY897
# Imberger and Patterson 1990

# typical spring wind speed: 10.3 miles per h 
10.3 * 1609.34 / 3600
opt.salt <- optim(par = 0.0, fn = optm_salt,
                  wtr.dat = 8,
                  wsp.dat = 4.6, bath.data = bath.data)
print(opt.salt$par)

out <- c()
# increase salt from 0 g/kg to 10 g/kg
inc_slt <- seq(0.0,10,0.001)
for (i in inc_slt){
  out <- rbind(out, plot_salt(i, wtr.dat = 8, wsp.dat = 4.6, bath.data = bath.data))
}
out <- as.data.frame(out)


g1 <- ggplot(out, aes(V1, V3/V2)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(opt.salt$par),linetype = 'dashed') +
  geom_hline(yintercept = 1,linetype = 'dashed') +
  # xlab('Uniformly added salt (g/kg) below center of volume')+
  # xlab('Salt (g/kg)')+
  xlab(bquote(Salinity ~ (g~kg^-1))) +
  ylab('LN (mass moment/wind moment)')+
  ggtitle('Spring conditions')+
  theme_minimal(base_size = 8); g1


plot_salt_3d <- function(x, wtr.dat = 12, wsp.dat = 1, bath.data = bath.data){
  temp_top = wtr.dat
  temp = temp_top  # deg C
  salt_top = 0
  salt = x # g/kg
  wnd = wsp.dat
  

  bth <- bath.data$Depth_meter # seq(from = 0, to = 25, by = 1)
  area <- bath.data$Area_meterSquared # seq(from = 39850000, to = 0, length.out = length(bth))
  zv <- bth %*% area / sum(area)
  wtr <- c(rep(temp_top, round(zv)), rep(temp, length(bth) - round(zv)))
  slt <- c(rep(get_dens(temp_top, salt_top), round(zv)), rep(get_dens(temp,salt), length(bth) - round(zv)))
  
  
  ssi = schmidt.stability(wtr = wtr, depths = bth, bthA = area, bthD = bth, sal = slt)
  
  wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd^2
  
  kinetic <- get_dens(temp = temp, salt = salt) *
    wnd_shear *
    area[1]^(3/2) *
    (max(bth) - zv)
  stable <- ssi * area[1] *
    (max(bth) - zv)
  
  # return(c(x, wsp.dat, kinetic, stable))
  
  bthA = area
  bthD = bth
  uStar = sqrt(wnd_shear)
  St = ssi
  metaT = zv
  metaB = 15
  averageHypoDense = get_dens(temp = temp, salt = salt) 
  g	<-	9.81
  dz	<-	0.1
  # if bathymetry has negative values, remove.
  # intepolate area and depth to 0
  Ao	<-	bthA[1]
  Zo	<-	bthD[1]
  if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
  #interpolates the bathymetry data
  layerD	<-	seq(Zo,max(bthD),dz)
  layerA	<-	stats::approx(bthD,bthA,layerD)$y
  #find depth to the center of volume
  Zv = layerD*layerA*dz                    
  Zcv = sum(Zv)/sum(layerA)/dz
  
  St_uC = St*Ao/g
  # Calculates the Lake Number according to the formula provided
  Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)
  
  return(c(x, wsp.dat, (2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv) ,g*St_uC*(metaT+metaB)  ))
  
}


inc_slt <- seq(0.0,30,0.1)
inc_wnd <- seq(0.0,30,0.1)
out.matrix <- matrix(NA, ncol = length(inc_slt), nrow = length(inc_wnd))
for (i in 1:length(inc_slt)){
  print(i)
  for (j in 1:length(inc_wnd)){
    print(j)
    out.matrix[j, i] = plot_salt_3d(inc_slt[i], wtr.dat = 8, wsp.dat = inc_wnd[j], bath.data = bath.data)[4]/plot_salt_3d(inc_slt[i], wtr.dat = 8, wsp.dat = inc_wnd[j], bath.data = bath.data)[3]
  }
}


out.contour <- data.frame('Salt' = inc_slt, 'Wind' =inc_wnd,
                          'LN_1' =  apply(out.matrix, 2, function(x) which.min(abs(x-1))),
                          'LN_2' =  apply(out.matrix, 2, function(x) which.min(abs(x-2))),
                          'LN_0.5' =  apply(out.matrix, 2, function(x) which.min(abs(x-0.5))),
                          'LN_10' =  apply(out.matrix, 2, function(x) which.min(abs(x-10))),
                          'LN_0.1' =  apply(out.matrix, 2, function(x) which.min(abs(x-0.1))))

m.out.contour <- reshape2::melt(out.contour, id.vars = c('Salt', 'Wind'))
m.out.contour = m.out.contour %>%
  mutate(label = if_else(Salt == max(Salt), as.character(variable), NA_character_)) 

m.out.contour$variable <- factor(m.out.contour$variable, levels = c('LN_0.1',
                                                                    'LN_0.5',
                                                                    'LN_1',
                                                                    'LN_2',
                                                                    "LN_10"))

g2 <- ggplot(m.out.contour, aes(x = Salt, y = Wind[value], col = variable)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(opt.salt$par), linetype = 'dashed') +
  geom_hline(yintercept = 4.6,linetype = 'dashed') +
  # xlab('Salt (g/kg)')+
  xlab(bquote(Salinity ~ (g~kg^-1))) +
  # ylab('Wind speed (m/s)')+
  ylab(bquote(Wind~Speed ~ (m~s^-1))) +
  scale_color_brewer(palette = 'Set1') +
  geom_curve(aes(x = 20, y = 18, xend = 8, yend = 28), curvature = 0.5, arrow = arrow(length = unit(0.1, "cm")), color = 'grey50', size = 0.2) +
  annotate(geom = 'text', label = 'turbulent', x = 20.5, y = 27, color = 'grey50', size = 2.5) +
  geom_curve(aes(x = 20, y = 16, xend = 23, yend = 7), curvature = -0.5, arrow = arrow(length = unit(0.1, "cm")), color = 'grey50', size = 0.2) +
  annotate(geom = 'text', label = 'stabilizing', x = 27, y = 10, color = 'grey50', size = 2.5) +
  ggtitle('Uniform 8°C')+
  # geom_label_repel(aes(label = label),
  #                  nudge_x = 1,
  #                  na.rm = TRUE, size = 2) +
  theme_minimal(base_size = 8)+
  xlim(0,30) + ylim(0,30)+ labs(color = '')+
  theme(legend.position = "right") ; g2

g <- g1 + g2 + plot_annotation(tag_levels = 'A') ;g 
ggsave(file = paste0(name_lake,'_theoretical.png'), g, dpi = 500, width = 6.5, height = 2.5, units = 'in')
ggsave(file = paste0(name_lake,'_theoretical.eps'), g, dpi = 500, width = 6.5, height = 2.5, units = 'in')

