# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

source("D:/Geodaten/Master/projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(sf)

################################################################################
# Import paths -----------------------------------------------------------------
################################################################################

# DATA (RASTERS)
# p = path

p.vv = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack"
p.vh = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack"
p.co = "F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.img"
p.dvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_dvi.vrt"
p.evi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_evi.vrt"
p.msavi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_msavi.vrt"
p.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.vrt"
p.reip = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_reip.vrt"
p.rvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_rvi.vrt"

# BANDNAMES
# n = naming

n.vv = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack.txt"
n.vh = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack.txt"
n.co = "F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.txt"
n.dvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_dvi.txt"
n.evi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_evi.txt"
n.msavi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_msavi.txt"
n.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.txt"
n.reip = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_reip.txt"
n.rvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_rvi.txt"

# vector files
path_gt = "D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg"

# auxillary folders
path_prediction = "D:/Geodaten/#Jupiter/GEO402/04_products/rf/"
path_developement = "D:/Geodaten/#Jupiter/GEO402/03_develop/"
path_tables = "D:/Geodaten/#Jupiter/GEO402/03_develop/ground_reference_files/"
path_vrt = paste0(path_developement, "s2/", "reflectance.vrt") # vrt path
path_vrt_co =  paste0(path_developement, "coherence/", "coherence.vrt")
path_vrt_co_warp =  paste0(path_developement, "coherence/", "coherence_warp.vrt")
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm_crop/bin_mask_less20.tif"
path_coherence = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/"
path_coherence_develop = "D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/"
path_rds = "D:/Geodaten/#Jupiter/GEO402/03_develop/rda/"

################################################################################
# Import and rename data -------------------------------------------------------
################################################################################

vh =    rename_bandnames(p.vh, n.vh)
vv =    rename_bandnames(p.vv, n.vv)
co =    rename_bandnames(p.co, n.co)
dvi =   rename_bandnames(p.dvi, n.dvi)
evi =   rename_bandnames(p.evi, n.evi)
msavi = rename_bandnames(p.msavi, n.msavi)
ndvi =  rename_bandnames(p.ndvi, n.ndvi)
reip =  rename_bandnames(p.reip, n.reip)
rvi =   rename_bandnames(p.rvi, n.rvi)


pred = brick("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn.tif")
pred_classif = raster("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn_classif.tif")

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read("D:/Geodaten/#Jupiter/GEO402/02_features/study_area.gpkg", layer = "final", quiet = TRUE) %>%  # read in
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read("D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm() %>%
    group_by(class_simple) %>%
    # CREATE RUNNING NUMBERS FOR GT
    mutate(id = row_number())

