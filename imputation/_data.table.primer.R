# Data imputation for classification model for land cover
# init: 05.11.2020, Konstantin Schellenberg

library(raster)
library(randomForestSRC)
library(tidyverse)
library(data.table)
library(purrr)
library(stars)
source("D:/Geodaten/Master/projects/402slangbos/functions.R")

# data.table primer ------------------------------------------------------------

p.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.vrt"
n.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.txt"
ndvi =  rename_bandnames(p.ndvi, n.ndvi)

ras = read_stars(p.ndvi)
ras = ndvi[[1:100]] %>% brick()
dt = as.data.table.raster(ras)

# get no of nas in dt
dt[is.na(ndvi.2016.03.11),ndvi.2016.03.11] %>% length()

# number of observations .N (special function)
dt[, .N]

# .() is .list() to call a j column
dt[, .(ndvi.2016.03.11)] # is a dt with 1 column
dt[, c(ndvi.2016.03.11)] # is a vector of the column

# chaining syntax [][][][] after another

# .SD (special function) for subset of data
dt[, print(.SD)]
# dt[, print(.SD), by = some identifier]

# using mappers for column-wise interactions
system.time(dt[, lapply(.SD, mean)])
system.time(dt[, map(.SD, mean)])
# ... same speed


# ANALYSIS ---------------------------------------------------------------------

# get sum of NA in the data
dt[, map(.SD, ~ sum(is.na(.x)))]
