library(glmtools)
library(patchwork)


# Load model output 
read.output <- function(csv, startyear){
  read_csv(csv) %>% 
    rename(DateTime = datetime) %>% 
    filter(DateTime >= as.Date(paste0(startyear,'-01-01'))) %>% 
    filter(DateTime < as.Date(paste0(startyear+2,'-01-01'))) 
}

getMixedDate.ME <- function(csvIce, csvDens, startyear){
  # Last ice day
  ice = read_csv(csvIce) %>% 
    rename(DateTime = datetime) %>% 
    filter(month(DateTime) <= 5 & year(DateTime) == startyear) %>% 
    filter(ice_height > 0) %>% 
    slice(n()) %>% 
    pull(DateTime)
  
  read_csv(csvDens) %>% 
    rename(DateTime = datetime) %>% 
    # pivot_longer(cols = -c(DateTime), names_to = 'depth', values_to = 'density') %>% 
    select(DateTime, dens_1, dens_24) %>% 
    filter(DateTime > ice) %>% # after ice comes off
    mutate(densDiff = dens_24 - dens_1) %>% 
    filter(densDiff <= 0.1) %>% 
    slice(1) %>% 
    pull(DateTime)
}
getMixedDate.MO <- function(csvIce, csvDens, startyear){
  # Last ice day
  ice = read_csv(csvIce) %>% 
    rename(DateTime = datetime) %>% 
    filter(month(DateTime) <= 5 & year(DateTime) == startyear) %>% 
    filter(ice_height > 0) %>% 
    slice(n()) %>% 
    pull(DateTime)
  
  # First mixed date
  read_csv(csvDens) %>% 
    rename(DateTime = datetime) %>% 
    select(DateTime, dens_1, dens_20) %>% 
    filter(DateTime > ice) %>% # after ice comes off
    mutate(densDiff = dens_20 - dens_1) %>% 
    filter(densDiff <= 0.1) %>% 
    slice(1) %>% 
    pull(DateTime)
}
# getMixedDate.ME('output_modelruns/1.5/Mendota_GLM_ice.csv', 
#              'output_modelruns/1.5/Mendota_GLM_dens.csv', 
#              2000)

# Plot model output 
plot.output <- function(df, title, var = 'temp', startyear, mixeddate1, mixeddate2, zlims = c(0,32), ylims = c(25,-0.5)) {
  plot_var_df(df, var_name = var, interpolate = T, zlim = zlims) +
    scale_y_reverse(expand = c(0.01,0.01), limits = ylims) +
    labs(title = title) +
    # Salt end ddate
    geom_vline(xintercept = as.numeric(as.POSIXct(paste0(startyear,'-05-01'))), linetype = 3, size = 0.2) +
    geom_vline(xintercept = as.numeric(as.POSIXct(paste0(startyear+1,'-05-01'))), linetype = 3, size = 0.2) +
    # Mixed dates
    geom_vline(xintercept = as.numeric(mixeddate1), linetype = 2, col = 'red4', size = 0.2) +
    geom_vline(xintercept = as.numeric(mixeddate2), linetype = 2, col = 'red4', size = 0.2) +
    annotate('text', label = format(mixeddate1, '%b-%d'), x = mixeddate1-(7*24*3600), y = 15, angle = 90, col = 'red4', size = 2) +
    annotate('text', label = format(mixeddate2, '%b-%d'), x = mixeddate2-(7*24*3600), y = 15, angle = 90, col = 'red4', size = 2) +
    theme_bw(base_size = 8) +
    theme(axis.title.x = element_blank(),
            legend.key.width = unit(0.3,'cm'))
}
# Test case
# plot.output(glm.temp.me, 'GLM temp 1.5', var = 'temp', startyear, glm.mixed1, glm.mixed2, zlims = c(0,32))

