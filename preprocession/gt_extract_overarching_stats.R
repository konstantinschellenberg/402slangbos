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

# load reference sites
gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm()

# ------------------------------------------------------------------------------

# example stats
b_extracted = exactextracting(gt, ras = coh[[1:10]], col_class = "class_simple", col_id = "id", statistics = c("mean", "stdev", "count"),
                    dstdir, outfile = "test.RDS")

# load dummy
b = readRDS("03_develop/extract/test.RDS")

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------
# USER INPUT -------------------------------------------------------------------


# all raster to be queried
rasters = list(vv, vh, red, nir, coh)

fun.ndvi(red[[1]], nir[[1]])
outfiles = sapply(c("vv", "vh", "red", "nir", "coh"), function(x) paste("extract", x, sep = "_"))


statistics = c("median", "sd", "mean")

# get example raster
ras = coh[[1:10]]
class = gt %>% filter(class_simple == "1")
col_class = "class_simple"

# RUN --------------------------------------------------------------------------

# example run
e = extract_summary(gt, coh[[1:10]], col_class = "class_simple", statistics = statistics)

# batch run
summary = list()
for (i in seq_along(rasters)){
    summary[[i]] = extract_summary(gt, rasters[[i]], "class_simple", statistics)
}

saveRDS(summary, paste0(dstdir, "summary_statistics.RDS"))

# (end)
