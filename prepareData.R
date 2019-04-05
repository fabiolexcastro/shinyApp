
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup
rm(list = ls())
g <- gc(reset = TRUE)
options(scipen = 999)
lbl <- data.frame(vls = 1:6, nms = paste0('Type ', 1:6))
vrs <- c(paste0('prec_', 1:12, '$'), paste0('tmax_', 1:12, '$'), paste0('tmean_', 1:12, '$'), paste0('tmin_', 1:12, '$'))
mnt <- data.frame(month_nmb = 1:12, month_nme = month.abb)

# Functions to use
tidyDF <- function(dfm, prd, gcm){
  # dfm <- vls_crn
  fnl <- dfm %>%
    mutate(id = 1:nrow(.)) %>%
    gather(var, value, -x, -y, -id) %>% 
    mutate(variable = str_sub(var, start = 1, end = 4),
           month = parse_number(var)) %>%
    dplyr::select(-var) %>%
    spread(variable, value) %>%
    mutate(period = prd,
           gcm = gcm)
  write.csv(fnl, paste0('data/tbl/vls_', prd, '.csv'), row.names = FALSE)
  print('Done!!!')
  return(fnl)
}
extGCM <- function(gc){
  gc <- paste0(gc, '/')
  fl <- grep(gc, fls, value = T)
}

# Load data
crn <- raster('data/suit_cocoa/tif/crn_cocoa.tif')
msk <- shapefile('data/shp/msk_suit.shp')

# Current
pth <- '//dapadfs/data_cluster_4/observed/gridded_products/worldclim/Global_10min'
fls <- list.files(pth, full.names = T) %>%
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
  mixedsort()
stk <- stack(fls)
stk_cut <- raster::crop(stk, msk) %>% raster::mask(., msk)

dir.create('data/tif/climate/crn', recursive = TRUE)
nms <- names(stk_cut)
stk_cut <- unstack(stk_cut)
Map('writeRaster', x = stk_cut, filename = paste0('data/tif/climate/crn/', nms, '.tif'), overwrite = FALSE)

# Raster to table
stk_cut <- stack(stk_cut)
vls_crn <- rasterToPoints(stk_cut)
vls_crn <- as_tibble(vls_crn)

# Tidy the dataframe
dfm_crn <- tidyDF(dfm = vls_crn, prd = 'current', gcm = 'current')

# Future
pth <- '//dapadfs/data_cluster_2/gcm/cmip5/downscaled/rcp60/global_10min'
gcm <- list.files(pth)
yrs <- c('2020_2049', '2040_2069')
fls <- list.files(paste(pth, gcm, 'r1i1p1', yrs[2], sep = '/'), full.names = TRUE) %>%
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
  mixedsort()

dfm_ftr <- lapply(1:length(gcm), function(k){
  print(gcm[k])
  g <- extGCM(gc = gcm[k])
  lyr <- stack(g)
  lyr <- raster::crop(lyr, msk) %>% raster::mask(., msk)
  print(paste0('Raster to Points ', gcm[k]))
  vls <- rasterToPoints(lyr) %>% as_tibble()
  fnl <- tidyDF(dfm = vls, prd = '2050', gcm = gcm[k])
  print('Done!')
  return(fnl)
})

dfm_ftr <- bind_rows(dfm_ftr)
write.csv(dfm_ftr, 'data/tbl/vls_2050.csv', row.names = FALSE)

# Join current and future tabla into only one
dfm_crn$period <- 'Current'
dfm_crn$gcm <- 'Current'

dfm_all <- rbind(dfm_crn, dfm_ftr)
dfm_all <- inner_join(x = dfm_all, y = mnt, by = c('month' = 'month_nmb'))
dfm_all <- dfm_all %>%
  dplyr::select(-month) %>% 
  rename(month = month_nme)
write.csv(dfm_all, 'data/tbl/vls_all.csv', row.names = FALSE)




