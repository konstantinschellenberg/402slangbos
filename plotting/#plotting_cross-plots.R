#' Example of cross-sensor data plotting on Slangbos Samples
#' Sentinel-1 and Sentinel-2
#' init: 24.09.2020, Konstantin Schellenberg


# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)

library(exactextractr)

options(max.print=50)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
proper_layernames = c("Sentinel-1 VH", "Sentinel-1 VV", "Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI",
                      "Sentinel-2 MSAVI", "Sentinel-2 NDVI", "Sentinel-2 REIP", "Sentinel-2 RVI")
proper_layernames.axis = c("S-1 VH [dB]", "S-1 VV [dB]", "S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index",
                           "S-2 MSAVI Index", "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"
plotdir = "06_plots/"

# READ IN ----------------------------------------------------------------------
dat = readRDS(paste0(dstdir, "summary_statistics.RDS"))
# NA is not omitted

# # clean data from NA (possible step in between)
# summary = map(dat, ~ map(.x, function(x){
#     na.omit(x)
# })
# )

################################################################################
# PLOTTING ---------------------------------------------------------------------

data = dat[["vh"]] %>% `names<-`(classnames)
# View(data)

diff.Date(data$Grassland$date)

a = data$`Slangbos Increase`$date
b = data$Cultivated$date

setdiff(a, b)
length(a);length(b)

unique(a[!a %in% b])

# clean data for same date

data

marker = list(size = 5)

plot_ly() %>%
    add_markers(data[["Slangbos Increase"]]$median, data[["Cultivated"]]$median, name = "Increase x Cultivated", opacity = 0.7,marker = marker) %>%
    add_markers(data[["Slangbos Increase"]]$median, data[["Grassland"]]$median, name = "Increase x Grassland", opacity = 0.7,marker = marker) %>%
    add_markers(data[["Slangbos Increase"]]$median, data[["Slangbos Clearning"]]$median, name = "Increase x Clearning", opacity = 0.7,marker = marker) %>%
    add_markers(data[["Slangbos Increase"]]$median, data[["Slangbos Continuous"]]$median, name = "Increase x Continuous", opacity = 0.7,marker = marker) %>%
    layout(title = "Sentinel-1 VH classwise data aggregation")

# this one meaningful for sample data!
plot_ly() %>%
    add_markers(data[["Slangbos Increase"]]$median, data[["Slangbos Increase"]]$count, name = "count dependence")
