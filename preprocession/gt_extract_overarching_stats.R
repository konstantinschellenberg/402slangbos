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

# complex or simplified data?
# 1 = selected classes, 2 = all sites
mode = 2

# be aware of using the gt or cgt samples data accordingly!

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------
# USER INPUT -------------------------------------------------------------------

if (mode == 1) classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
if (mode == 2) classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated",
                              "Bare Soil", "Woodland", "Urban", "Water")

# metadata
col_class = "class_simple"
col_id = "id"

if (mode == 1) dstdir = "03_develop/extract/"
if (mode == 2) dstdir = "03_develop/extract_all/"


# RUN --------------------------------------------------------------------------

# example raster
ras = ndvi[[1:30]]
# example run
example = extract_summary(cgt, ras, col_class)

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

if (mode == 1) summary = map(rasters, ~ extract_summary(gt, .x, col_class))
if (mode == 2) summary = map(rasters, ~ extract_summary(cgt, .x, col_class))
saveRDS(summary, paste0(dstdir, "summary_statistics.RDS"))
# summary = readRDS("03_develop/extract/summary_statistics.RDS")

# (end)
