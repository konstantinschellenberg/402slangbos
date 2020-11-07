# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

source("D:/Projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(purrr)
library(sf)

################################################################################
# Import paths -----------------------------------------------------------------
################################################################################

# DATA (RASTERS)
# p = path

p.vh = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack"
p.co = "F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.img"
p.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.vrt"
p.savi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_savi.vrt"

p.chrips = "F:/geodata/geo402/CHIRPS/FreeState_Ladybrand_14days_2015_2020.tif"

# BANDNAMES
# n = naming

n.vh = "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack.txt"
n.co = "F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.txt"
n.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.txt"
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

# loads raster (takes a few seconds)
vh =    rename_bandnames(p.vh, n.vh)
co =    rename_bandnames(p.co, n.co)
ndvi =  rename_bandnames(p.ndvi, n.ndvi)
savi =  rename_bandnames(p.savi, n.savi)

rasters = list(co, ndvi, savi, vh)

layernames = c("co", "ndvi", "savi", "vh")
names(rasters) = layernames
