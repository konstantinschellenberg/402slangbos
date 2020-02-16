# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

getOption("max.print")
options(digits = 4, max.print = 1000)
source("functions.R")

library(tidyverse)
library(tidytable)
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(ggplot2)
library(data.table)

library(plotly)
library(leaflet)
library(processx)

library(mlr3verse)
library(precrec)
library(mlr3spatiotempcv)
library(parallelMap)
library(rasterVis)
library(RColorBrewer)
library(paradox)
library(mlr3tuning)

################################################################################
# Import paths -----------------------------------------------------------------
################################################################################

# raw data
s1vv_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VV_free_state_study_area_geo402_fillna_crop"
s1vh_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VH_free_state_study_area_geo402_fillna_crop"
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
path_naming_s2 = paste0(path_s2, "bandnames_less20.txt")
path_naming_s1 = paste0(path_s1, "s1_bandnames.txt")
path_rds = "D:/Geodaten/#Jupiter/GEO402/03_develop/rda/"

################################################################################
# Import and rename data -------------------------------------------------------
################################################################################

vv = brick(s1vv_path) %>% rename_bandnames(option = 1, var_prefix = "vv", naming = path_naming_s1)
vh = brick(s1vh_path) %>% rename_bandnames(option = 1, var_prefix = "vh", naming = path_naming_s1)

red = brick(s2red) %>% rename_bandnames(option = 3, var_prefix = "red", naming = path_naming_s2)
nir = brick(s2nir) %>% rename_bandnames(option = 3, var_prefix = "nir", naming = path_naming_s2)
# cm = brick(path_cm) %>%  rename_bandnames(option = 3, var_prefix = "cm", naming = path_naming_s2)

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read(path_gt, layer = "study_area", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read(path_gt, layer = "gt", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE) %>% # Remove Z-Dimension
    dplyr::group_by(Name) %>%
    mutate(number = row_number()) # adding row numbers to classes


