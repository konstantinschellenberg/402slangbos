# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

options(digits = 4, max.print = 50)
source("functions.R")

library(tidyverse)
library(raster)
library(sf)

################################################################################
# Import Sentinel-1 time series data--------------------------------------------
################################################################################

# raw data
s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"
s2red = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/red.tif"
s2nir = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/nir.tif"

# paths
prediction_out_path = "D:\\Geodaten\\#Jupiter\\GEO402\\04_products\\rf\\"
path_developement = "D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\"
path_vrt = paste0(path_developement, "s2\\", "reflectance.vrt") # vrt path
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm/"

# where to write rds_files:
rds_path = "D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\rda\\"

s1vv = brick(s1vv_path)
s1vh = brick(s1vh_path)
red = brick(s2red)
nir = brick(s2nir)

s1vh_small = brick("D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\raster_small_vh.tif")
names(s1vh_small) = names(s1vh) # pre-rename because dataset was created in QGIS with automatic naming

# Revome invalid raster (covering less than half of the area)-------------------
s1vv = rename_bandnames(raster = s1vv) %>%
    .[[c(-14, -17, -62)]] # remove brocken files

s1vh = rename_bandnames(raster = s1vh) %>%
    .[[c(-14, -17, -62)]]

s1vh_small = rename_bandnames(raster = s1vh_small) %>%
    .[[c(-14, -17, -62)]]


################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

gt_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\features.gpkg"

gt = st_read(gt_path, layer = "ROI_updated", quiet = TRUE) %>%  # read in
    st_transform(st_crs(s1vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

study_area = st_read(gt_path, layer = "study_area", quiet = TRUE) %>%  # read in
    st_transform(st_crs(s1vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension


# check if class is sf, crs is South African projection!
