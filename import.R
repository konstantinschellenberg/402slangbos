# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

options(digits = 4)
source("functions.R")

library(tidyverse)
library(raster)
library(sf)

################################################################################
# Import Sentinel-1 time series data--------------------------------------------
################################################################################

s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"
s2 = ""

# where to write rds_files:
rds_path = "D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\rda\\"

s1vv = brick(s1vv_path)
s1vh = brick(s1vh_path)

# Revome invalid raster (covering less than half of the area)-------------------
s1vv = rename_bandnames(raster = s1vv) %>%
    .[[c(-14, -17, -62)]]

s1vh = rename_bandnames(raster = s1vh) %>%
    .[[c(-14, -17, -62)]]

# test dataset
raster_test = s1vv[[seq(1,3)]]

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

gt_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"

gt = st_read(gt_path, quiet = TRUE) %>%  # read in
    st_transform(st_crs(s1vv)) %>%  # set crs(gt) to the crs(s1) brick.
    st_zm(drop = TRUE)  # Remove Z-Dimension

# check if class is sf, crs is South African projection!
