library(scales)
library(glmtools)

source('3_plotModel_functions.R')
startyear = 2013 #Choose start year. Set up to plot two years

# Read in ice data
ice = read_csv('data/ntl_icedatescombo.csv')
# Mendota temperature observations
me.obs = plot_var_df(read.output('output_modelruns/NULL/Mendota_Obs_temp.csv', startyear), var_name = 'Temp', interpolate = T, zlim = c(0,32),) +
  # add ice rect. 
  geom_rect(data = ice %>% filter(lakeid == 'ME', year == startyear-1), aes(xmin = as.POSIXct(paste0(startyear,'-01-01')), xmax = as.POSIXct(lastice), ymin = -0.5, ymax = 0.5), 
            col = 'black',fill = 'grey80', size = 0.1) +
  # add ice rect. 
  geom_rect(data = ice %>% filter(lakeid == 'ME', year == startyear), aes(xmin = as.POSIXct(firstice), xmax = as.POSIXct(lastice), ymin = -0.5, ymax = 0.5), 
            col = 'black',fill = 'grey80', size = 0.1) +
  labs(title = 'Lake Mendota: Temp Observations') +
  theme_bw(base_size = 8) +
  scale_y_reverse(expand = c(0.01,0.01), limits = c(25,-0.5)) +
  scale_x_datetime(limits = c(as.POSIXct(paste0(startyear,'-01-01')), as.POSIXct(paste0(startyear+2,'-01-01'))),
                   expand = c(0.01,0.01)) +
  theme(axis.title.x = element_blank(),
        legend.key.width = unit(0.3,'cm')); me.obs

# Monona temperature observations
mo.obs = plot_var_df(read.output('output_modelruns/NULL/Monona_Obs_temp.csv', startyear), var_name = 'Temp', interpolate = T, zlim = c(0,32)) +
  # add ice rect. 
  geom_rect(data = ice %>% filter(lakeid == 'MO', year == startyear-1), aes(xmin = as.POSIXct(paste0(startyear,'-01-01')), xmax = as.POSIXct(lastice), ymin = -0.5, ymax = 0.5), 
            col = 'black',fill = 'grey80', size = 0.1) +
  # add ice rect. 
  geom_rect(data = ice %>% filter(lakeid == 'MO', year == startyear), aes(xmin = as.POSIXct(firstice), xmax = as.POSIXct(lastice), ymin = -0.5, ymax = 0.5), 
            col = 'black',fill = 'grey80', size = 0.1) +
  labs(title = 'Lake Monona: Temp Observations') +
  scale_y_reverse(expand = c(0.01,0.01), limits = c(22,-0.5)) +
  scale_x_datetime(limits = c(as.POSIXct(paste0(startyear,'-01-01')), as.POSIXct(paste0(startyear+2,'-01-01'))),
                   expand = c(0.01,0.01)) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank(),
        legend.key.width = unit(0.3,'cm')); me.obs


#### GLM #####
me.t.glm.NULL = plot.output(read.output('output_modelruns/NULL/Mendota_GLM_temp.csv', startyear), 
                         var = 'Temp', 
                         'Lake Mendota: GLM temp @ 0 g/kg', startyear, 
                         getMixedDate.ME('output_modelruns/NULL/Mendota_GLM_ice.csv', 'output_modelruns/NULL/Mendota_GLM_dens.csv', startyear), 
                         getMixedDate.ME('output_modelruns/NULL/Mendota_GLM_ice.csv', 'output_modelruns/NULL/Mendota_GLM_dens.csv', startyear+1),
                         zlims = c(0,32))
me.t.glm.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_GLM_temp.csv', startyear), 
                        'Lake Mendota: GLM temp @ 1.5 g/kg', startyear, 
                        var = 'Temp', 
                        getMixedDate.ME('output_modelruns/1.5/Mendota_GLM_ice.csv', 'output_modelruns/1.5/Mendota_GLM_dens.csv', startyear), 
                        getMixedDate.ME('output_modelruns/1.5/Mendota_GLM_ice.csv', 'output_modelruns/1.5/Mendota_GLM_dens.csv', startyear+1),
                        zlims = c(0,32))
me.s.glm.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_GLM_salt.csv', startyear), 
                        'Lake Mendota: GLM salinity @ 1.5 g/kg', startyear, 
                        var = 'Salinity', 
                        getMixedDate.ME('output_modelruns/1.5/Mendota_GLM_ice.csv', 'output_modelruns/1.5/Mendota_GLM_dens.csv', startyear), 
                        getMixedDate.ME('output_modelruns/1.5/Mendota_GLM_ice.csv', 'output_modelruns/1.5/Mendota_GLM_dens.csv', startyear+1),
                        zlims = c(0,1.5))

mo.t.glm.NULL = plot.output(read.output('output_modelruns/NULL/Monona_GLM_temp.csv', startyear), 
                         'Lake Monona: GLM temp @ 0 g/kg', startyear, 
                         var = 'Temp', 
                         getMixedDate.MO('output_modelruns/NULL/Monona_GLM_ice.csv', 'output_modelruns/NULL/Monona_GLM_dens.csv', startyear), 
                         getMixedDate.MO('output_modelruns/NULL/Monona_GLM_ice.csv', 'output_modelruns/NULL/Monona_GLM_dens.csv', startyear+1),
                         zlims = c(0,32), ylims = c(21,-0.5))
mo.t.glm.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_GLM_temp.csv', startyear), 
                        'Lake Monona: GLM temp @ 1.5 g/kg', startyear, 
                        var = 'Temp',
                        getMixedDate.MO('output_modelruns/1.5/Monona_GLM_ice.csv', 'output_modelruns/1.5/Monona_GLM_dens.csv', startyear), 
                        getMixedDate.MO('output_modelruns/1.5/Monona_GLM_ice.csv', 'output_modelruns/1.5/Monona_GLM_dens.csv', startyear+1),
                        zlims = c(0,32), ylims = c(21,-0.5))
mo.s.glm.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_GLM_salt.csv', startyear), 
                        'Lake Monona: GLM salinity @ 1.5 g/kg', startyear, 
                        var = 'Salinity',
                        getMixedDate.MO('output_modelruns/1.5/Monona_GLM_ice.csv', 'output_modelruns/1.5/Monona_GLM_dens.csv', startyear), 
                        getMixedDate.MO('output_modelruns/1.5/Monona_GLM_ice.csv', 'output_modelruns/1.5/Monona_GLM_dens.csv', startyear+1),
                        zlims = c(0,1.5), ylims = c(21,-0.5))

layout = "
AAABBB
CCCDDD
EEEFFF
GGGHHH
"
me.obs + mo.obs +
me.t.glm.NULL + mo.t.glm.NULL +
me.t.glm.1.5 + mo.t.glm.1.5 +
me.s.glm.1.5 + mo.s.glm.1.5 +
plot_layout(design = layout, guides = 'collect')
ggsave(paste0('figs_HD/plotModelOutputs_',startyear,'_GLM.png'), width = 6.5, height = 5, units = 'in', dpi = 500)
ggsave(paste0('figs_HD/figureS03.pdf'), width = 6.5, height = 5, units = 'in', dpi = 500)
       
#### GOTM #####
me.t.GOTM.NULL = plot.output(read.output('output_modelruns/NULL/Mendota_GOTM_temp.csv', startyear), 
                            var = 'Temp', 
                            'Lake Mendota: GOTM temp @ 0 g/kg', startyear, 
                            getMixedDate.ME('output_modelruns/NULL/Mendota_GOTM_ice.csv', 'output_modelruns/NULL/Mendota_GOTM_dens.csv', startyear), 
                            getMixedDate.ME('output_modelruns/NULL/Mendota_GOTM_ice.csv', 'output_modelruns/NULL/Mendota_GOTM_dens.csv', startyear+1),
                            zlims = c(0,32))
me.t.GOTM.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_GOTM_temp.csv', startyear), 
                           'Lake Mendota: GOTM temp @ 1.5 g/kg', startyear, 
                           var = 'Temp', 
                           getMixedDate.ME('output_modelruns/1.5/Mendota_GOTM_ice.csv', 'output_modelruns/1.5/Mendota_GOTM_dens.csv', startyear), 
                           getMixedDate.ME('output_modelruns/1.5/Mendota_GOTM_ice.csv', 'output_modelruns/1.5/Mendota_GOTM_dens.csv', startyear+1),
                           zlims = c(0,32))
me.s.GOTM.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_GOTM_salt.csv', startyear), 
                           'Lake Mendota: GOTM salinity @ 1.5 g/kg', startyear, 
                           var = 'Salinity', 
                           getMixedDate.ME('output_modelruns/1.5/Mendota_GOTM_ice.csv', 'output_modelruns/1.5/Mendota_GOTM_dens.csv', startyear), 
                           getMixedDate.ME('output_modelruns/1.5/Mendota_GOTM_ice.csv', 'output_modelruns/1.5/Mendota_GOTM_dens.csv', startyear+1),
                           zlims = c(0,1.5))

mo.t.GOTM.NULL = plot.output(read.output('output_modelruns/NULL/Monona_GOTM_temp.csv', startyear), 
                            'Lake Monona: GOTM temp @ 0 g/kg', startyear, 
                            var = 'Temp', 
                            getMixedDate.MO('output_modelruns/NULL/Monona_GOTM_ice.csv', 'output_modelruns/NULL/Monona_GOTM_dens.csv', startyear), 
                            getMixedDate.MO('output_modelruns/NULL/Monona_GOTM_ice.csv', 'output_modelruns/NULL/Monona_GOTM_dens.csv', startyear+1),
                            zlims = c(0,32), ylims = c(21,-0.5))
mo.t.GOTM.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_GOTM_temp.csv', startyear), 
                           'Lake Monona: GOTM temp @ 1.5 g/kg', startyear, 
                           var = 'Temp',
                           getMixedDate.MO('output_modelruns/1.5/Monona_GOTM_ice.csv', 'output_modelruns/1.5/Monona_GOTM_dens.csv', startyear), 
                           getMixedDate.MO('output_modelruns/1.5/Monona_GOTM_ice.csv', 'output_modelruns/1.5/Monona_GOTM_dens.csv', startyear+1),
                           zlims = c(0,32), ylims = c(21,-0.5))
mo.s.GOTM.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_GOTM_salt.csv', startyear), 
                           'Lake Monona: GOTM salinity @ 1.5 g/kg', startyear, 
                           var = 'Salinity',
                           getMixedDate.MO('output_modelruns/1.5/Monona_GOTM_ice.csv', 'output_modelruns/1.5/Monona_GOTM_dens.csv', startyear), 
                           getMixedDate.MO('output_modelruns/1.5/Monona_GOTM_ice.csv', 'output_modelruns/1.5/Monona_GOTM_dens.csv', startyear+1),
                           zlims = c(0,1.5), ylims = c(21,-0.5))

layout = "
AAABBB
CCCDDD
EEEFFF
GGGHHH
"
me.obs + mo.obs +
  me.t.GOTM.NULL + mo.t.GOTM.NULL +
  me.t.GOTM.1.5 + mo.t.GOTM.1.5 +
  me.s.GOTM.1.5 + mo.s.GOTM.1.5 +
  plot_layout(design = layout, guides = 'collect')
ggsave(paste0('figs_HD/plotModelOutputs_',startyear,'_GOTM.png'), width = 6.5, height = 5, units = 'in', dpi = 500)
ggsave(paste0('figs_HD/figureS04.pdf'), width = 6.5, height = 5, units = 'in', dpi = 500)
       
#### Simstrat #####
me.t.Simstrat.NULL = plot.output(read.output('output_modelruns/NULL/Mendota_Simstrat_temp.csv', startyear), 
                             var = 'Temp', 
                             'Lake Mendota: Simstrat temp @ 0 g/kg', startyear, 
                             getMixedDate.ME('output_modelruns/NULL/Mendota_Simstrat_ice.csv', 'output_modelruns/NULL/Mendota_Simstrat_dens.csv', startyear), 
                             getMixedDate.ME('output_modelruns/NULL/Mendota_Simstrat_ice.csv', 'output_modelruns/NULL/Mendota_Simstrat_dens.csv', startyear+1),
                             zlims = c(0,32))
me.t.Simstrat.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_Simstrat_temp.csv', startyear), 
                            'Lake Mendota: Simstrat temp @ 1.5 g/kg', startyear, 
                            var = 'Temp', 
                            getMixedDate.ME('output_modelruns/1.5/Mendota_Simstrat_ice.csv', 'output_modelruns/1.5/Mendota_Simstrat_dens.csv', startyear), 
                            getMixedDate.ME('output_modelruns/1.5/Mendota_Simstrat_ice.csv', 'output_modelruns/1.5/Mendota_Simstrat_dens.csv', startyear+1),
                            zlims = c(0,32))
me.s.Simstrat.1.5 = plot.output(read.output('output_modelruns/1.5/Mendota_Simstrat_salt.csv', startyear), 
                            'Lake Mendota: Simstrat salinity @ 1.5 g/kg', startyear, 
                            var = 'Salinity', 
                            getMixedDate.ME('output_modelruns/1.5/Mendota_Simstrat_ice.csv', 'output_modelruns/1.5/Mendota_Simstrat_dens.csv', startyear), 
                            getMixedDate.ME('output_modelruns/1.5/Mendota_Simstrat_ice.csv', 'output_modelruns/1.5/Mendota_Simstrat_dens.csv', startyear+1),
                            zlims = c(0,1.5))

mo.t.Simstrat.NULL = plot.output(read.output('output_modelruns/NULL/Monona_Simstrat_temp.csv', startyear), 
                             'Lake Monona: Simstrat temp @ 0 g/kg', startyear, 
                             var = 'Temp', 
                             getMixedDate.MO('output_modelruns/NULL/Monona_Simstrat_ice.csv', 'output_modelruns/NULL/Monona_Simstrat_dens.csv', startyear), 
                             getMixedDate.MO('output_modelruns/NULL/Monona_Simstrat_ice.csv', 'output_modelruns/NULL/Monona_Simstrat_dens.csv', startyear+1),
                             zlims = c(0,32), ylims = c(21,-0.5))
mo.t.Simstrat.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_Simstrat_temp.csv', startyear), 
                            'Lake Monona: Simstrat temp @ 1.5 g/kg', startyear, 
                            var = 'Temp',
                            getMixedDate.MO('output_modelruns/1.5/Monona_Simstrat_ice.csv', 'output_modelruns/1.5/Monona_Simstrat_dens.csv', startyear), 
                            getMixedDate.MO('output_modelruns/1.5/Monona_Simstrat_ice.csv', 'output_modelruns/1.5/Monona_Simstrat_dens.csv', startyear+1),
                            zlims = c(0,32), ylims = c(21,-0.5))
mo.s.Simstrat.1.5 = plot.output(read.output('output_modelruns/1.5/Monona_Simstrat_salt.csv', startyear), 
                            'Lake Monona: Simstrat salinity @ 1.5 g/kg', startyear, 
                            var = 'Salinity',
                            getMixedDate.MO('output_modelruns/1.5/Monona_Simstrat_ice.csv', 'output_modelruns/1.5/Monona_Simstrat_dens.csv', startyear), 
                            getMixedDate.MO('output_modelruns/1.5/Monona_Simstrat_ice.csv', 'output_modelruns/1.5/Monona_Simstrat_dens.csv', startyear+1),
                            zlims = c(0,1.5), ylims = c(21,-0.5))

layout = "
AAABBB
CCCDDD
EEEFFF
GGGHHH
"
me.obs + mo.obs +
me.t.Simstrat.NULL + mo.t.Simstrat.NULL +
  me.t.Simstrat.1.5 + mo.t.Simstrat.1.5 +
  me.s.Simstrat.1.5 + mo.s.Simstrat.1.5 +
  plot_layout(design = layout, guides = 'collect')
ggsave(paste0('figs_HD/plotModelOutputs_',startyear,'_Simstrat.png'), width = 6.5, height = 5, units = 'in', dpi = 500)
ggsave(paste0('figs_HD/figureS05.pdf'), width = 6.5, height = 5, units = 'in', dpi = 500)



#### Ice off plots ####
me.iceoff = getIceOff('output_modelruns/1.5/Mendota_GLM_ice.csv') %>% mutate(model = 'GLM') %>% 
  bind_rows(
    getIceOff('output_modelruns/1.5/Mendota_GOTM_ice.csv') %>% mutate(model = 'GOTM')
  ) %>% bind_rows(
    getIceOff('output_modelruns/1.5/Mendota_Simstrat_ice.csv') %>% mutate(model = 'Simstrat')
  )

i1 = ice %>% filter(lakeid == 'ME' & year >= 1995 & year(lastice) <= 2015) %>% select(lastice) %>% 
  mutate(year = year(lastice)) %>% left_join(me.iceoff) %>% 
  ggplot() +
  geom_point(aes(x = as.Date(yday(lastice), origin = as.Date('2019-01-01')), 
                 y = as.Date(yday(iceoff), origin = as.Date('2019-01-01')), group = model)) +
  geom_abline() +
  ylab('Modeled ice-off (DOY)') + xlab('Observed ice-off (DOY)') +
  scale_x_date(labels = date_format("%b"), breaks = 'month') +
  scale_y_date(labels = date_format("%b"), breaks = 'month') +
  facet_wrap(~model) +
  labs(title = 'Lake Mendota') +
  theme_bw(base_size = 8) 

mo.iceoff = getIceOff('output_modelruns/1.5/Monona_GLM_ice.csv') %>% mutate(model = 'GLM') %>% 
  bind_rows(
    getIceOff('output_modelruns/1.5/Monona_GOTM_ice.csv') %>% mutate(model = 'GOTM')
  ) %>% bind_rows(
    getIceOff('output_modelruns/1.5/Monona_Simstrat_ice.csv') %>% mutate(model = 'Simstrat')
  )

i2 = ice %>% filter(lakeid == 'MO' & year >= 1995 & year(lastice) <= 2015) %>% select(lastice) %>% 
  mutate(year = year(lastice)) %>% left_join(mo.iceoff) %>% 
  ggplot() +
  geom_point(aes(x = as.Date(yday(lastice), origin = as.Date('2019-01-01')), 
                 y = as.Date(yday(iceoff), origin = as.Date('2019-01-01')), group = model)) +
  geom_abline() +
  ylab('Modeled ice-off (DOY)') + xlab('Observed ice-off (DOY)') +
  scale_x_date(labels = date_format("%b"), breaks = 'month') +
  scale_y_date(labels = date_format("%b"), breaks = 'month') +
  facet_wrap(~model) +
  labs(title = 'Lake Monona') +
  theme_bw(base_size = 8) 
i1/i2
ggsave('figs_HD/ice_off_comparison.png', dpi = 500, width = 5,height = 5, units = 'in')


