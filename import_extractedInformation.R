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
