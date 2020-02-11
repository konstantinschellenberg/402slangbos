# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

options(digits = 4, max.print = 300)
source("functions.R")

library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(ggplot2)
library(data.table)

library(mlr3verse)
library(precrec)
library(mlr3spatiotempcv)
library(parallelMap)


################################################################################
# Import paths -----------------------------------------------------------------
################################################################################

# raw data
s1vv_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VH_free_state_study_area_geo402.tif"
s1vh_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VH_free_state_study_area_geo402_fillna_crop.tif"
old_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VH_free_state_study_area_geo402" #-14, -17, -62 invalid!
s2red = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/red_less20_gapfilled_realdates_spline.tif"
s2nir = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/nir_less20_gapfilled_realdates_spline.tif"

path_gt = "D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg"

# paths
path_prediction = "D:/Geodaten/#Jupiter/GEO402/04_products/rf/"
path_developement = "D:/Geodaten/#Jupiter/GEO402/03_develop/"
path_vrt = paste0(path_developement, "s2/", "reflectance.vrt") # vrt path
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm_crop/bin_mask_less20.tif"
path_naming = paste0(path_s2, "bandnames_less20.txt")

################################################################################
# Import and rename data -------------------------------------------------------
################################################################################

# where to write rds_files:
rds_path = "D:/Geodaten/#Jupiter/GEO402/03_develop/rda/"

olds1 = brick(old_s1)
vv = brick(s1vv_path) %>% rename_bandnames(option = 1, var_prefix = "vv", naming = olds1)
vh = brick(s1vh_path) %>% rename_bandnames(option = 1, var_prefix = "vh", naming = olds1)

red = brick(s2red) %>% rename_bandnames(option = 3, var_prefix = "red", naming = paste0(path_s2, "bandnames_less20.txt"))
nir = brick(s2nir) %>% rename_bandnames(option = 3, var_prefix = "nir", naming = paste0(path_s2, "bandnames_less20.txt"))
cm = brick(path_cm) %>%  rename_bandnames(option = 3, var_prefix = "cm", naming = paste0(path_s2, "bandnames_less20.txt"))

# vh_small = brick(paste0(path_s1, "vh_small.tif")) %>% rename_bandnames(option = 2, var_prefix = "vh", naming = olds1)
# red_small = brick(paste0(path_s2, "red_small.tif")) %>% rename_bandnames(option = 2, var_prefix = "red", naming = paste0(path_s2, "bandnames.txt"))
# nir_small = brick(paste0(path_s2, "nir_small.tif")) %>% rename_bandnames(option = 2, var_prefix = "nir", naming = paste0(path_s2, "bandnames.txt"))

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read(path_gt, layer = "study_area", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read(path_gt, layer = "gt", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE) # Remove Z-Dimension

# gt_smaller = sf::st_crop(gt, st_bbox(extent_smaller))
