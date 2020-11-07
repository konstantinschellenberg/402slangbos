# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

source("D:/Projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(purrr)
library(sf)


layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "savi", "vh", "vv")
proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI", "Sentinel-2 MSAVI", "Sentinel-2 NDVI",
                      "Sentinel-2 REIP", "Sentinel-2 RVI", "Sentinel-2 SAVI", "Sentinel-1 VH", "Sentinel-1 VV")
proper_layernames.axis = c("S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index", "S-2 MSAVI Index",
                           "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index", "S-2 SAVI Index", "S-1 VH [dB]", "S-1 VV [dB]")

classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
classnames_all = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated",
                   "Bare Soil", "Woodland", "Urban", "Water")

################################################################################
# Extracted Information (not tidy table, but lists in lists) -------------------
################################################################################

files = list.files("D:/Geodaten/GEO402/03_develop/extract", "^extract", full.names =  TRUE)
files = files[!grepl("all", files)]
extract = map(files, ~ readRDS(.x)) %>% `names<-`(layernames)
extract = extract %>%
    `names<-`(layernames) %>%
    map( ~ `names<-`(.x, classnames))
master = readRDS("D:/Geodaten/GEO402/03_develop/extract/extract_all.RDS")

summary = readRDS("D:/Geodaten/GEO402/03_develop/extract/summary_statistics.RDS")
summary_all = readRDS("D:/Geodaten/GEO402/03_develop/extract_all/summary_statistics.RDS")
