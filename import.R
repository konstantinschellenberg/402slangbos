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
library(rlist)

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
s1vh_co = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/covh"
s1vv_co = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/covv"
s1vh_co_nulls = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/covh_nulls"
s1vv_co_nulls = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/covv_nulls"

path_gt = "D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg"

# paths
path_prediction = "D:/Geodaten/#Jupiter/GEO402/04_products/rf/"
path_developement = "D:/Geodaten/#Jupiter/GEO402/03_develop/"
path_vrt = paste0(path_developement, "s2/", "reflectance.vrt") # vrt path
path_vrt_co =  paste0(path_developement, "coherence/", "coherence.vrt")
path_vrt_co_warp =  paste0(path_developement, "coherence/", "coherence_warp.vrt")
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm_crop/bin_mask_less20.tif"
path_coherence = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/"
path_coherence_develop = "D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/"
path_naming_s2 = paste0(path_s2, "bandnames_less20.txt")
path_naming_s1 = paste0(path_s1, "s1_bandnames.txt")
path_naming_co = paste0(path_coherence, "coherence_bandnames.txt")
path_naming_co_nulls = paste0(path_coherence, "coherence_bandnames_nulls.txt")
path_rds = "D:/Geodaten/#Jupiter/GEO402/03_develop/rda/"

################################################################################
# Import and rename data -------------------------------------------------------
################################################################################

vv = brick(s1vv_path) %>% rename_bandnames(option = 1, var_prefix = "vv", naming = path_naming_s1)
vh = brick(s1vh_path) %>% rename_bandnames(option = 1, var_prefix = "vh", naming = path_naming_s1)

red = brick(s2red) %>% rename_bandnames(option = 3, var_prefix = "red", naming = path_naming_s2)
nir = brick(s2nir) %>% rename_bandnames(option = 3, var_prefix = "nir", naming = path_naming_s2)

covh = brick(s1vh_co) %>% rename_bandnames(option = 4, var_prefix = "covh", naming = path_naming_co)
covv = brick(s1vv_co) %>% rename_bandnames(option = 4, var_prefix = "covv", naming = path_naming_co)
# cm = brick(path_cm) %>%  rename_bandnames(option = 3, var_prefix = "cm", naming = path_naming_s2)

covh_all = brick(s1vh_co_nulls) %>% rename_bandnames(option = 5, var_prefix = "covh_all", naming = path_naming_co_nulls)
covv_all = brick(s1vv_co_nulls) %>% rename_bandnames(option = 5, var_prefix = "covv_all", naming = path_naming_co_nulls)

vh_Q95 = raster("D:/Geodaten/#Jupiter/GEO402/03_develop/multitemp/VH_Q95.tif")
vh_Q05 = raster("D:/Geodaten/#Jupiter/GEO402/03_develop/multitemp/VH_Q05.tif")
vh_med = raster("D:/Geodaten/#Jupiter/GEO402/03_develop/multitemp/VH_med.tif")

pred = brick("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn.tif")
pred_classif = raster("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn_classif.tif")

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read(path_gt, layer = "study_area", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

# only training gt
gt = st_read(path_gt, layer = "gt", quiet = TRUE) %>%  # read in
    st_transform(st_crs(vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE) %>% # Remove Z-Dimension
    dplyr::group_by(Name) %>%
    mutate(number = row_number()) %>%  # adding row numbers to classes
    mutate(descrip = case_when(Name == 1 ~ "Slangbos Increase", # adding descriptions
                                   Name == 2 ~ "Slangbos Continuous",
                                   Name == 3 ~ "Slangbos Breakpoint",
                                   Name == 4 ~ "Agriculture",
                                   Name == 5 ~ "Bare Soil",
                                   Name == 6 ~ "Grassland",
                                   Name == 7 ~ "Forest",
                                   Name == 8 ~ "Urban",
                                   Name == 9 ~ "Water"))

# including validation gt
all_gt = read_sf(path_gt, layer = "gt_total") %>%
    st_zm() %>%
    st_transform(st_crs(vv))
