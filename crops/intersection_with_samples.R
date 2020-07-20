# Intersect Crop Types and Turnover scores with Slangbos samples

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)

options(max.print = 100, )

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# LOAD DATASETS ----------------------------------------------------------------

data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple")
# gt$break_date = as.POSIXct(gt$break_date, format = "%m%Y", optional = T)

inter = st_intersection(gt, data)

st_write(inter, "02_features/Ladybrand_CropData.gpkg", layer = "SampleIntersection")

# now visualize crop type in sentinel timeseries...
