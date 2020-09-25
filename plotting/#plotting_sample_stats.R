#' Plotting Sample Statistics revisited
#' init: 24.09.2020

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
library(data.table)

library(exactextractr)

options(max.print=100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(co, dvi, evi, msavi, ndvi, reip, rvi, vh, vv)
layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "vh", "vv")
# proper_layernames = c("Sentinel-1 VH", "Sentinel-1 VV", "Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI",
#                       "Sentinel-2 MSAVI", "Sentinel-2 NDVI", "Sentinel-2 REIP", "Sentinel-2 RVI")
# proper_layernames.axis = c("S-1 VH [dB]", "S-1 VV [dB]", "S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index",
#                            "S-2 MSAVI Index", "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
plotdir = "06_plots/"

# READ IN MASTER DATA ----------------------------------------------------------

master = readRDS("03_develop/extract/extract_all.RDS")

# PLOTTING ------------------------------------------------------------------------------

