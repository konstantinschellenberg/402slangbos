# Script to calculate general stats of the classes

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)

library(exactextractr)

options(max.print = 200)

source("D:/Projects/402slangbos/import_rasters_extraction.R")
source("D:/Projects/402slangbos/functions.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/GEO402"
setwd(env)

# complex or simplified data?
# 1 = selected classes, 2 = all sites
mode = 2

# be aware of using the gt or cgt samples data accordingly!

# STATISTICS FOR ALL CLASSES AGGREGATED ----------------------------------------
# USER INPUT -------------------------------------------------------------------

# get gt object
sample = st_read("D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "classif_2017-2018") %>%
    group_by(classif) %>%
    mutate(id = row_number())

# metadata
col_class = "classif"
col_id = "id"

dstdir = "03_develop/extract_classif/"
if (!dir.exists(dstdir)) dir.create(dstdir)

# RUN --------------------------------------------------------------------------

# example raster
ras = ndvi[[1:30]]
# example run
example = extract_summary(sample, ras, col_class)

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

summary = map(rasters, ~ extract_summary(sample, .x, col_class))

saveRDS(summary, paste0(dstdir, "summary_statistics.RDS"))
# summary = readRDS("03_develop/extract/summary_statistics.RDS")

# (end)
