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
# source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm()

# IMPORT RASTERS ---------------------------------------------------------------

vh = brick("F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack")
indizes.names = c("ndvi", "evi", "dvi", "reip", "msavi", "rvi") %>% sort()
indizes.files = list.files("F:/geodata/geo402/S2/xx_S2_indices/mosaics", "*.vrt", full.names = TRUE)
indizes = map(indizes.files, ~ brick(.x)) %>% `names<-`(indizes.names)
# vv = brick("F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack")
co = brick("F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.img")

# TODO:
# all bands have to be renamed!

textfiles.dir = "F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH"
textfiles = list.files(textfiles.dir, "*.txt", full.names = TRUE)

csv = read_csv(textfiles[1])
ras = indizes[[1]]

names(ras) = csv

map2(indizes, textfiles, ~ `names<-`(.x, read_csv(.y)))

bandnames("F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/stack_35JMH_dvi.img", prefix = "dvi", writeout = TRUE)

# CREATE RUNNING NUMBERS FOR GT ------------------------------------------------

gt = gt %>%
    group_by(class_simple) %>%
    mutate(id = row_number())

# SOME METADATA FOR TESTING ----------------------------------------------------

# metadata
col_class = "class_simple"
col_id = "id"
dstdir = "03_develop/extract/"

# layernames of raster need to have a date suffix in the form yyyy.mm.dd
rasters = indizes %>% c(vh, co) %>% `names<-`(c(indizes.names, "vh", "co"))
outfiles = sapply(c(indizes.names, "vh", "co"), function(x) paste("extract", x, sep = "_"))
outfile = "test"

# median is calculated anyways; must be coercable by exact_extract()
statistics = c("mean", "stdev", "count")

#' TODO:
#' calc supsmus stats only when not NA

# EXTRACTING -------------------------------------------------------------------
b = exactextracting(gt, co[[1:5]], col_class, col_id, statistics, dstdir, "outfile")

for (i in seq_along(outfiles)){
    exactextracting(gt = gt, ras = rasters[[i]],
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = outfiles[i])
}

# READ IN ----------------------------------------------------------------------

co = readRDS("03_develop/extract/extract_ndvi")

map(co[[1]], ~ sum(is.na(.x)))

o = co[[1]][[44]]
is.na(o)

o[is.na(o)] = NA


ggplot(o) +
    geom_point(aes(date, med)) +
    geom_line(aes(date, med)) +
    geom_ribbon(aes(date, ymin = mean - 2*stdev, ymax = mean + 2* stdev)) +
    geom_line(aes(date, med), color = "red") +
    theme_bw()

plot_ly(o) %>% plotly::add_lines(x = ~date, y= ~med, connectgaps = F)
