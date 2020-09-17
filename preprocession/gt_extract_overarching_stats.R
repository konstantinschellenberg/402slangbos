# Script to calculate general stats of the classes

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

# destination
dstdir = "03_develop/extract/"

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------
# USER INPUT -------------------------------------------------------------------

# all raster to be queried
rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
names(rasters) = layernames

# RUN --------------------------------------------------------------------------

# example raster
ras = ndvi[[1:30]]
# example run
example = extract_summary(gt, ras, col_class)

o = example[[1]]

# POINTS
ggplot(o) +
    geom_point(aes(date, median), color = "red") +
    geom_point(aes(date, med_smooth), color = "blue")

# LINE
# with NA lines
ggplot(o) +
    geom_line(aes(date, median), color = "red") +
    geom_line(aes(date, med_smooth), color = "blue")

# without NA lines
o2 = o %>% na.omit()
ggplot(o2) +
    geom_line(aes(date, median), color = "red") +
    geom_line(aes(date, med_smooth), color = "blue")


# batch run --------------------------------------------------------------------

summary = map(rasters, ~ extract_summary(gt, .x, col_class))
saveRDS(summary, paste0(dstdir, "summary_statistics.RDS"))
# summary = readRDS("03_develop/extract/summary_statistics.RDS")

# (end)
