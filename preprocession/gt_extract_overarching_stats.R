# Script to calculate general stats of the classes

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)

library(exactextractr)

options(max.print = 200)

source("D:/Geodaten/Master/projects/402slangbos/functions.R")
source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"

# TEST RUN ---------------------------------------------------------------------

# example raster
ras = co[[1:5]]
col_class = "class_simple"

# check if exactextracting is operative
example_stats = exactextracting(gt, ras, col_class,
                                col_id = "id", statistics = c("mean", "stdev", "count"),
                                dstdir, outfile = "test.RDS")

# load dummy
readRDS("03_develop/extract/test.RDS")

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------
# USER INPUT -------------------------------------------------------------------

# all raster to be queried
rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
names(rasters) = layernames

statistics = c("median", "sd", "mean")

# RUN --------------------------------------------------------------------------

################ Baustelle

# example run
e = extract_summary(gt, ras, col_class, statistics)

# batch run
summary = list()
for (i in seq_along(rasters)){
    summary[[i]] = extract_summary(gt, rasters[[i]], col_class, statistics)
}

saveRDS(summary, paste0(dstdir, "summary_statistics.RDS"))

# (end)
