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

# all rasters

################################################################################
mode = 2
################################################################################


    p.vv = "F:/geodata/geo402/S1_GRD/S1A_IW_GRD_VV_stack"
    p.vh = "F:/geodata/geo402/S1_GRD/S1A_IW_GRD_VH_stack"
    p.co = "F:/geodata/geo402/S1_SLC/S1A_IW_SLC_VV_stack.img"
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

    n.vv = "F:/geodata/geo402/S1_GRD/S1A_IW_GRD_VV_stack.txt"
    n.vh = "F:/geodata/geo402/S1_GRD/S1A_IW_GRD_VH_stack.txt"
    n.co = "F:/geodata/geo402/S1_SLC/S1A_IW_SLC_VV_stack.txt"
    n.dvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_dvi.txt"
    n.evi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_evi.txt"
    n.msavi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_msavi.txt"
    n.ndvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_ndvi.txt"
    n.reip = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_reip.txt"
    n.rvi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_rvi.txt"
    n.savi = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/stack_savi.txt"

    # vector files
    path_gt = "D:/Geodaten/GEO402/02_features/features.gpkg"

    # auxillary folders
    path_prediction = "D:/Geodaten/GEO402/04_products/rf/"
    path_developement = "D:/Geodaten/GEO402/03_develop/"
    path_tables = "D:/Geodaten/GEO402/03_develop/ground_reference_files/"
    path_vrt = paste0(path_developement, "s2/", "reflectance.vrt") # vrt path
    path_s2 = "D:/Geodaten/GEO402/01_data/s2/"
    path_s1 = "D:/Geodaten/GEO402/01_data/s1_data/"
    path_coherence = "D:/Geodaten /GEO402/01_data/coherence/"
    path_coherence_develop = "D:/Geodaten/GEO402/03_develop/coherence/"
    path_rds = "D:/Geodaten/GEO402/03_develop/rda/"

    ################################################################################
    # Import and rename data -------------------------------------------------------
    ################################################################################

if (mode == 1){
    # loads raster (takes a few seconds)
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

    # layer names for pixel extraction routines
    layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "savi", "vh", "vv")
    proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI", "Sentinel-2 MSAVI", "Sentinel-2 NDVI",
                          "Sentinel-2 REIP", "Sentinel-2 RVI", "Sentinel-2 SAVI", "Sentinel-1 VH", "Sentinel-1 VV")
    proper_layernames.axis = c("S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index", "S-2 MSAVI Index",
                               "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index", "S-2 SAVI Index", "S-1 VH [dB]", "S-1 VV [dB]")
    names(rasters) = layernames
    classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
    classnames_all = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated",
                       "Bare Soil", "Woodland", "Urban", "Water")
    cat("Loaded all raster layers")
}

if (mode == 2){

    # loads raster (takes a few seconds)
    vh =    rename_bandnames(p.vh, n.vh)
    co =    rename_bandnames(p.co, n.co)
    ndvi =  rename_bandnames(p.ndvi, n.ndvi)
    savi =  rename_bandnames(p.savi, n.savi)

    rasters = list(co, ndvi, savi, vh)

    # layer names for pixel extraction routines
    layernames = c("co", "ndvi", "savi", "vh")
    proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 NDVI", "Sentinel-2 SAVI", "Sentinel-1 VH")
    proper_layernames.axis = c("S-1 VV Coherence", "S-2 NDVI Index", "S-2 SAVI Index", "S-1 VH [dB]")

    names(rasters) = layernames
    classnames = c('Slangbos', 'Pasture', 'Grassland', 'Cultivated', 'Bare', 'Woodland', 'Urban', 'Water')

    cat("Loaded raster layers for classification")
}




