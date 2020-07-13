# gt = ground-truth samples

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

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm()

# CREATE RUNNING NUMBERS FOR GT ------------------------------------------------


# already done
# gt = gt %>%
#     group_by(class_simple) %>%
#     mutate(id = row_number())

# SOME METADATA FOR TESTING ----------------------------------------------------

# metadata
col_class = "class_simple"
col_id = "id"
dstdir = "03_develop/extract/"
rasters = list(vv, vh, red, nir, covv_all)
outfiles = sapply(c("vv", "vh", "red", "nir", "coh"), function(x) paste("extract", x, sep = "_"))
outfile = "test"
statistics = c("mean", "stdev", "count") # must be coercable by exact_extract()

#' layernames of raster need to have a date suffix in the form yyyy.mm.dd
#'
#'
#' TODOs:
#' calc supsmus stats only when not NA

# EXTRACTING -------------------------------------------------------------------
b = exactextracting(gt, coh[[1:5]], col_class, col_id, statistics, dstdir, "outfile")

for (i in seq_along(outfiles)){
    exactextracting(gt = gt, ras = rasters[[i]],
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = outfiles[i])
}

# READ IN ----------------------------------------------------------------------

co = readRDS("03_develop/extract/extract_coh")

map(co[[1]], ~ sum(is.na(.x)))

o = co[[5]][[15]]
is.na(o)

o[is.na(o)] = NA


ggplot(o) +
    geom_point(aes(date, mean)) +
    geom_line(aes(date, med)) +
    geom_ribbon(aes(date, ymin = mean - stdev, ymax = mean + stdev)) +
    geom_line(aes(date, med), color = "red") +
    theme_bw()

plot_ly(o) %>% plotly::add_lines(x = ~date, y= ~mean, connectgaps = F)
