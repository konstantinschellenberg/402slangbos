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

options(max.print=999999)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
proper_layernames = c("S-1 VH", "S-1 VV", "S-1 VV Coherence", "S-2 DVI", "S-2 EVI",
                       "S-2 MSAVI", "S-2 NDVI", "S-2 REIP", "S-2 RVI")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"
plotdir = "06_plots/"

# READ IN ----------------------------------------------------------------------
data = readRDS(paste0(dstdir, "summary_statistics.RDS"))

# clean data from NA (possible step in between)
summary = map(data, ~ map(.x, function(x){
    na.omit(x)
})
)


# GGPLOTS SIMPLE ---------------------------------------------------------------
p.vh = summary[[1]]
p.vv = summary[[2]]
p.co = summary[[3]]
p.ndvi = summary[["ndvi"]]

# Slangbos increase VH
ggplot(p.vh[[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth))

# Slangbos increase COH
ggplot(p.co[[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth))

# Slangbos increase NDVI
map(p.ndvi, ~ ggplot(.x) +
    geom_point(aes(date, median))+
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth)))

ggplot(p.co[[2]], aes(x = date)) +
    geom_line(aes(y = median)) +
    geom_line(aes(y = med_smooth)) +
    geom_ribbon(aes(ymin = losd_smooth, ymax = upsd_smooth), fill = "grey", alpha = 0.5) +
    theme_minimal()

arr = list()
for (i in seq_along(rasters)){
    summary_of_raster = summary[[i]]

    out = map2(summary_of_raster, classnames, function(x, y) ggplot(x, aes(date, median)) +
        # geom_smooth(method = "lm", color = "grey60", se = F) +
        geom_line(aes(x = date, y = med_smooth), size = 0.2) +
        geom_ribbon(aes(x = date, ymin = losd_smooth, ymax = upsd_smooth), alpha = 0.2, fill = "blue") +
        ggtitle(y) +
        theme_minimal())
    arr[[i]] = out
}

# arrange plot in grobs (lattice-like grid cells)
arr = map2(arr, proper_layernames, ~ marrangeGrob(.x, nrow=3, ncol=2, top = .y))
map(arr, ~ print(.x))

# save the plots
map2(arr, proper_layernames, ~ ggsave(filename = paste0(.y, ".png"), path = plotdir, plot = .x, width = 10, height = 10))

# PLOTY COMPLEX ----------------------------------------------------------------
