# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

print(commandArgs())

if (!exists("args")) {
    suppressPackageStartupMessages(library("argparse"))
    parser <- ArgumentParser()
    parser$add_argument("-a", "--arg1", type="integer", default=1,
                        help="Mode [default %(default)s]")
    args <- parser$parse_args()
}

source("D:/Projects/402slangbos/functions.R")

library(tidyverse)
library(raster)
library(purrr)
library(sf)

# mode 1 = old reference site and data
# mode 2 = classification data with new sampling

mode = commandArgs()

if (mode == 1){

    ### USER INPUT
    path.extracts = "D:/Geodaten/GEO402/03_develop/extract"
    ###

    layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "savi", "vh", "vv")
    proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI", "Sentinel-2 MSAVI", "Sentinel-2 NDVI",
                          "Sentinel-2 REIP", "Sentinel-2 RVI", "Sentinel-2 SAVI", "Sentinel-1 VH", "Sentinel-1 VV")
    proper_layernames.axis = c("S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index", "S-2 MSAVI Index",
                               "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index", "S-2 SAVI Index", "S-1 VH [dB]", "S-1 VV [dB]")

    classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
    classnames_all = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated",
                       "Bare Soil", "Woodland", "Urban", "Water")

    cat("Loading (old) extracted data frames")

}

if (mode == 2){

    ### USER INPUT
    path.extracts = "D:/Geodaten/GEO402/03_develop/extract_classif"
    ###

    layernames = c("co", "ndvi", "savi", "vh")
    proper_layernames = c("Sentinel-1 VV Coherence", "Sentinel-2 NDVI", "Sentinel-2 SAVI", "Sentinel-1 VH")
    proper_layernames.axis = c("S-1 VV Coherence", "S-2 NDVI Index", "S-2 SAVI Index", "S-1 VH [dB]")

    names(rasters) = layernames
    classnames = c('Slangbos', 'Pasture', 'Grassland', 'Cultivated', 'Bare', 'Woodland', 'Urban', 'Water')

    cat("Loading extracted information for classification")

}

################################################################################
# Extracted Information (not tidy table, but lists in lists) -------------------
################################################################################

files = list.files(path.extracts, "^extract", full.names =  TRUE)

extract = map(files, ~ readRDS(.x)) %>% `names<-`(layernames)
extract = extract %>%
    `names<-`(layernames) %>%
    map( ~ `names<-`(.x, classnames))

master = readRDS(file.path(path.extracts, "master.RDS"))
summary = readRDS(file.path(path.extracts, "summary_statistics.RDS"))

# all, not automatised
summary_all = readRDS("D:/Geodaten/GEO402/03_develop/extract_all/summary_statistics.RDS")




