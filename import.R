# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

source("D:/Geodaten/Master/projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(purrr)
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
p.savi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_savi.vrt"

p.chrips = "F:/geodata/geo402/CHIRPS/FreeState_Ladybrand_14days_2015_2020.tif"

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
n.savi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_savi.txt"

# vector files
path_gt = "D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg"

# auxillary folders
path_prediction = "D:/Geodaten/#Jupiter/GEO402/04_products/rf/"
path_developement = "D:/Geodaten/#Jupiter/GEO402/03_develop/"
path_tables = "D:/Geodaten/#Jupiter/GEO402/03_develop/ground_reference_files/"
path_vrt = paste0(path_developement, "s2/", "reflectance.vrt") # vrt path
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/"
path_s1 = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/"
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
savi =  rename_bandnames(p.savi, n.savi)

rasters = list(co, dvi, evi, msavi, ndvi, reip, rvi, savi, vh, vv)

layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "savi", "vh", "vv")
proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI", "Sentinel-2 MSAVI", "Sentinel-2 NDVI",
                      "Sentinel-2 REIP", "Sentinel-2 RVI", "Sentinel-2 SAVI", "Sentinel-1 VH", "Sentinel-1 VV")
proper_layernames.axis = c("S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index", "S-2 MSAVI Index",
                           "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index", "S-2 SAVI Index", "S-1 VH [dB]", "S-1 VV [dB]")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
classnames_all = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated",
               "Bare Soil", "Woodland", "Urban", "Water")


pred = brick("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn.tif")
pred_classif = raster("D:/Geodaten/#Jupiter/GEO402/04_products/rf/run1_vrn_classif.tif")

################################################################################
# Extracted Information (not tidy table, but lists in lists) -------------------
################################################################################

files = list.files("D:/Geodaten/#Jupiter/GEO402/03_develop/extract", "^extract", full.names =  TRUE)
files = files[!grepl("all", files)]
extract = map(files, ~ readRDS(.x)) %>% `names<-`(layernames)
extract = extract %>%
    `names<-`(layernames) %>%
    map( ~ `names<-`(.x, classnames))

summary = readRDS("D:/Geodaten/#Jupiter/GEO402/03_develop/extract/summary_statistics.RDS")
summary_all = readRDS("D:/Geodaten/#Jupiter/GEO402/03_develop/extract_all/summary_statistics.RDS")

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read("D:/Geodaten/#Jupiter/GEO402/02_features/study_area.gpkg", layer = "final", quiet = TRUE) %>%  # read in
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read("D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm() %>%
    group_by(class_simple) %>%
    # CREATE RUNNING NUMBERS FOR GT
    mutate(id = row_number()) %>%
    dplyr::ungroup()

cgt = st_read("D:/Geodaten/#Jupiter/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full_simple") %>%
    st_zm() %>%
    group_by(class_simple) %>%
    # CREATE RUNNING NUMBERS FOR GT
    mutate(id = row_number()) %>%
    dplyr::ungroup()
