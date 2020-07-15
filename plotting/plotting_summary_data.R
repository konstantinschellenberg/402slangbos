# PLotting summary statistics

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)

library(exactextractr)

options(max.print = 200)

source("D:/Geodaten/Master/projects/402slangbos/functions.R")
source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"
plotdir = "06_plots/"

# load reference sites
gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm()

# READ IN ----------------------------------------------------------------------
s = readRDS(paste0(dstdir, "summary_statistics.RDS")) %>%
    `names<-`(c("vv", "vh", "red", "nir", "coh"))

# GGPLOTS SIMPLE ---------------------------------------------------------------

name = c("S1 VV", "S1 VH", "S2 RED", "S2 NIR", "S1 VV Coherence")

# Slangbos increase COH
ggplot(s[[5]][[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median))

# Slangbos increase VH
ggplot(s[[1]][[1]]) +
    geom_point(aes(date, median))+
    geom_line(aes(date, median))

# calculate ndvi
red_median = map(s[[3]], function(x) x$median)
nir_median = map(s[[4]], function(x) x$median)
ndvi = map2(red_median, nir_median, ~ fun.ndvi(.x, .y))


arr = list()
for (i in seq_along(name)){
    met = s[[i]]
    out = map2(met, name, function(x, y) ggplot(x, aes(date, median)) +
        geom_smooth(method = "lm", color = "red") +
        geom_smooth(color = "blue") +
        geom_line(aes(x = supsmu(date, median)$x, y = supsmu(date, median)$y), size = 1) +
        geom_point() +
        ggtitle(y) +
        theme_minimal())
    arr[[i]] = out
}

gname = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")
arr = map2(arr, gname, ~ marrangeGrob(.x, nrow=3, ncol=2, top = .y))
arr1 = marrangeGrob(arr[[1]], nrow = 3, ncol = 2, top = "Slangbos Increase")

map2(arr, gname, ~ ggsave(filename = paste0(.y, ".png"), path = plotdir, plot = .x, width = 10, height = 10))

# PLOTY COMPLEX ----------------------------------------------------------------
