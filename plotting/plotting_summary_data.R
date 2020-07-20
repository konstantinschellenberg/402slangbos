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
    `names<-`(c("vv", "vh", "ndvi", "coh"))

# GGPLOTS SIMPLE ---------------------------------------------------------------

name = c("S1 VV", "S1 VH", "S2 NDVI", "S1 VV Coherence")
gname = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")


# Slangbos increase COH
ggplot(s[[4]][[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median))

# Slangbos increase VH
ggplot(s[[1]][[1]]) +
    geom_point(aes(date, median))+
    geom_line(aes(date, median))

# Slangbos increase NDVI
map(s[[3]], ~ ggplot(.x) +
    geom_point(aes(date, median))+
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth)))

ggplot(s[[2]][[5]], aes(x = date)) +
    geom_line(aes(y = median)) +
    geom_line(aes(y = med_smooth)) +
    geom_ribbon(aes(ymin = losd_smooth, ymax = upsd_smooth), fill = "grey70", alpha = 0.2) +
    theme_minimal()

# calculate ndvi
# red_median = map(s[[3]], function(x) x$median)
# nir_median = map(s[[4]], function(x) x$median)
# ndvi = map2(red_median, nir_median, ~ fun.ndvi(.x, .y))


arr = list()
for (i in seq_along(name)){
    met = s[[i]]
    out = map2(met, gname, function(x, y) ggplot(x, aes(date, median)) +
        geom_smooth(method = "lm", color = "grey60", se = F) +
        # geom_smooth(color = "blue") +
        geom_line(aes(x = date, y = med_smooth), size = 0.2) +
        geom_ribbon(aes(x = date, ymin = losd_smooth, ymax = upsd_smooth), alpha = 0.2, fill = "blue") +
        ggtitle(y) +
        theme_minimal())
    arr[[i]] = out
}

arr = map2(arr, name, ~ marrangeGrob(.x, nrow=3, ncol=2, top = .y))
arr[[3]];arr[[1]];arr[[2]];arr[[4]]

map2(arr, name, ~ ggsave(filename = paste0(.y, ".png"), path = plotdir, plot = .x, width = 10, height = 10))

# PLOTY COMPLEX ----------------------------------------------------------------
