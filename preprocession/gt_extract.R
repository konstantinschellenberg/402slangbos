# gt = ground-truth samples

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(mapview)
library(ggplot2)

library(exactextractr)

options(max.print = 100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm()

# CREATE RUNNING NUMBERS FOR GT ------------------------------------------------

gt = gt %>%
    group_by(class_simple) %>%
    mutate(id = row_number())


# ------------------------------------------------------------------------------

# metadata
metrics = c("mean", "stdev", "count") # must be coercable by exact_extract()
col_class = "class_simple"
col_id = "id"

dstdir = "03_develop/extract/"

rasters = list(vv, vh, red, nir, covv_all)
outfiles = sapply(c("vv", "vh", "red", "nir", "coh"), function(x) paste("extract", x, sep = "_"))

#' layernames of raster need to have a date suffix in the form yyyy.mm.dd
#'

for (i in seq_along(outfiles)){
    exactextracting(gt = gt, ras = rasters[[i]],
                    col_class = "class_simple",
                    col_id = "id",
                    stats = metrics,
                    dstdir = dstdir,
                    outfile = outfiles[i])
}

# READ IN ----------------------------------------------------------------------

co = readRDS("03_develop/extract/extract_coh")

map(co[[1]], ~ sum(is.na(.x)))

o = co[[1]][[34]]

ggplot(o) +
    geom_point(aes(date, mean)) +
    geom_line(aes(date, med)) +
    geom_ribbon(aes(date, ymin = mean - stdev, ymax = mean + stdev)) +
    geom_line(aes(date, med), color = "red") +
    theme_bw()
