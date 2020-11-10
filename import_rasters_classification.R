# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

source("D:/Projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(purrr)
library(sf)
library(data.table)

################################################################################
# Import paths -----------------------------------------------------------------
################################################################################

# DATA (RASTERS)
# p = path

p.vh = "D:/Geodaten/GEO402/01_data/S1_GRD/S1A_IW_GRD_VH_stack"
p.co = "D:/Geodaten/GEO402/01_data/S1_SLC/S1A_IW_SLC_VV_stack"
p.ndvi = "D:/Geodaten/GEO402/01_data/S2/ndvi"
p.savi = "D:/Geodaten/GEO402/01_data/S2/savi"
p.cube1 = "D:/Geodaten/GEO402/01_data/stack/datecube_2015-10-01_filled.envi"
p.cube2 = "D:/Geodaten/GEO402/01_data/stack/datecube_2017-10-01_filled.envi"
p.cube3 = "D:/Geodaten/GEO402/01_data/stack/datecube_2019-10-01_filled.envi"

# BANDNAMES
# n = naming

n.vh = "D:/Geodaten/GEO402/01_data/S1_GRD/S1A_IW_GRD_VH_stack.txt"
n.co = "D:/Geodaten/GEO402/01_data/S1_SLC/S1_A_VV_stack_coherence_full_area.txt"
n.ndvi = "D:/Geodaten/GEO402/01_data/S2/ndvi.txt"
n.savi = "D:/Geodaten/GEO402/01_data/S2/savi.txt"
n.cube = ""

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

cubes = map(list(p.cube1, p.cube2, p.cube3), ~ brick(.x))



