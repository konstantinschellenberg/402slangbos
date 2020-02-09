# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

options(digits = 4, max.print = 150)
source("functions.R")

library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)

################################################################################
# Import Sentinel-1 time series data--------------------------------------------
################################################################################

# raw data
s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402.tif"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402.tif"
old_s1 = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402" #-14, -17, -62 invalid!
s2red = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/red.tif"
s2nir = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/nir.tif"

path_gt = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\features.gpkg"

# paths
prediction_out_path = "D:\\Geodaten\\#Jupiter\\GEO402\\04_products\\rf\\"
path_developement = "D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\"
path_vrt = paste0(path_developement, "s2\\", "reflectance.vrt") # vrt path
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm/"


# where to write rds_files:
rds_path = "D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\rda\\"

naming = brick(old_s1)
vv = brick(s1vv_path) %>% rename_bandnames(sentinel = 1, var_prefix = "vv", naming_raster = naming)
vh = brick(s1vh_path) %>% rename_bandnames(sentinel = 1, var_prefix = "vh", naming_raster = naming)
red = brick(s2red) %>% rename_bandnames(sentinel = 2, var_prefix = "red")
nir = brick(s2nir) %>% rename_bandnames(sentinel = 2, var_prefix = "nir")

vh_small = brick(paste0(path_s1, "vh_small.tif")) %>% rename_bandnames(sentinel = 1, var_prefix = "vh", naming_raster = naming)
red_small = brick(paste0(path_s2, "red_small.tif")) %>% rename_bandnames(sentinel = 2, var_prefix = "red")
nir_small = brick(paste0(path_s2, "nir_small.tif")) %>% rename_bandnames(sentinel = 2, var_prefix = "nir")

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read(gt_path, layer = "study_area", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

extent_smaller = st_read(gt_path, layer = "smaller_extent", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read(gt_path, layer = "gt", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE) # Remove Z-Dimension

gt_smaller = sf::st_crop(gt, st_bbox(smaller_extent))


