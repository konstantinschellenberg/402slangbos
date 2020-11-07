# Import Chirps Data


library(tidyverse)
library(sf)
library(raster)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

p.chp14 = "F:/geodata/geo402/CHIRPS/FreeState_Ladybrand_14days_2015_2020_32732.tif"
p.chpD = "F:/geodata/geo402/CHIRPS/FreeState_Ladybrand_daily_2015-_32732.tif"
p.chpM = "F:/geodata/geo402/CHIRPS/FreeState_Ladybrand_monthly_2015_2020_32732.tif"

# IMPORT -----------------------------------------------------------------------

chp14 = brick(p.chp14)
chpM = brick(p.chpM)
chpD = brick(p.chpD)

# PRE DAY ---------------------------------------------------------------------

plot(chpD[[7]])
nlayers(chpD)

# create layernames
dates = seq(from = as.Date("2015/1/1"), by = 1, length.out = nlayers(chpD))
names(chpD) = paste0("chpD.", dates)

# Point extract: Grep only where is gt
df = exact_extract(chpD, gt)


# PLOTTING ---------------------------------------------------------------------


plts11 %>%
    add_bars()


fort = chpD %>% as.data.frame(xy = TRUE)

ggplot() +
    geom_tile(data = fort, aes(x, y ,fill = chpD.2018.01.08)) +
    geom_sf(data = sf::st_transform(gt, 4326)) +
    theme_minimal()
