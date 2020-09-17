# gt = ground-truth samples

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)

library(exactextractr)

options(max.print = 200)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# IMPORT RASTERS ---------------------------------------------------------------

rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
names(rasters) = layernames

layernames = layernames[8:9]
rasters = list(rasters[[8]], rasters[[9]]) %>% `names<-`(layernames)

# SOME METADATA FOR TESTING ----------------------------------------------------

# metadata
col_class = "class_simple"
col_id = "id"
dstdir = "03_develop/extract/"

# layernames of raster need to have a date suffix in the form yyyy.mm.dd

outfiles = sapply(layernames, function(x) paste("extract", x, sep = "_"))
outfile = "test"

# median is calculated anyways; must be coercable by exact_extract()
statistics = c("mean", "stdev", "count")

# EXTRACTING -------------------------------------------------------------------
b = exactextracting(gt, co[[1:10]], col_class, col_id, statistics, dstdir, "outfile")

map2(outfiles, rasters, function(x, y){
    cat(x)
    exactextracting(gt = gt, ras = y,
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = x)
})

# reip 3-15 PROBLEM

# example READ IN ----------------------------------------------------------------------

example = readRDS("03_develop/extract/extract_reip")

# map(ras[[1]], ~ sum(is.na(.x)))

o = example[[1]][[5]]
o[is.na(o)] = NA
o = o %>% na.omit()

ggplot(o) +
    geom_ribbon(aes(date, ymin = mean - 2*stdev, ymax = mean + 2* stdev), fill = "lightgrey", alpha = 0.9) +
    geom_line(aes(date, med)) +
    geom_line(aes(date, med_smooth), color = "blue") +
    geom_line(aes(date, losd_smooth), na.rm = T) +
    # geom_line(aes(date, mean + 2 * stdev), na.rm = T) +
    theme_bw()

plot_ly(o) %>% plotly::add_lines(x = ~date, y= ~med, connectgaps = T)
