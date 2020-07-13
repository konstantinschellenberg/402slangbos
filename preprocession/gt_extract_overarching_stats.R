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

# example stats
b_extracted = exactextracting(gt, ras = coh[[1:10]], col_class = "class_simple", col_id = "id", statistics = c("mean", "stdev", "count"),
                    dstdir, outfile = "test.RDS")

# load dummy
b = readRDS("03_develop/extract/test.RDS")

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------

# all raster to be queried
rasters = list(vv, vh, red, nir, coh)
outfiles = sapply(c("vv", "vh", "red", "nir", "coh"), function(x) paste("extract", x, sep = "_"))


statistics = c("median", "sd", "mean")

# get example raster
ras = coh[[1:10]]
class = gt %>% filter(class_simple == "1")
col_class = "class_simple"






extract_summary(gt, coh[[1:50]], col_class = "class_simple", statistics = statistics)

summary = list()
for (i in seq_along(rasters)){
    summary[[i]] = extract_summary(gt, rasters[[i]], "class_simple", statistics)
}


# ------------------------------------------------------------------------------

# from the median we want the median and stdev of all sites
# init single dataframe


# add dates
stat = cbind(stat, date)
stat$median = as.numeric(stat$median)

stat = stat %>% mutate(med_smooth = ifelse(!is.na(median), yes = supsmu(date, median - sd)$y, no = NA),
                losd_smooth = ifelse(!is.na(median), yes = supsmu(date, mean - sd)$y, no = NA),
                upsd_smooth = ifelse(!is.na(median), yes = supsmu(date, mean + sd)$y, no = NA))

outer[[i]] = stat



for (i in seq_along(outfiles)){
    exactextracting(gt = gt, ras = rasters[[i]],
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = outfiles[i])
}
