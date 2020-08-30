#' creating VRTs
#'

library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)

# USER INPUT -------------------------------------------------------------------

dirs = c("F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/",
         "F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JNH/")
# output automatically created

outputdir = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/output.vrt"

# PROCESSING -------------------------------------------------------------------

# get directories only of files ending with "img" (= raster information)
files = map(dirs, ~ list.files(.x, full.names = T, recursive = T, pattern = "img$"))

# create bandnames .txt
for (i in seq_along(files)){
    map(files[[i]], function(x) {
        split = str_split(x, "_")
        prefix = map_chr(split, ~ .x[length(.x)])
        prefix2 = map(prefix, ~ str_sub(.x, 1, -5))
        bandnames(x, prefix = prefix2)
    })

}

# MAIN PROBLEM, not matching count of scenes
nlayers(rasters[[1]][[4]])
nlayers(rasters[[2]][[4]])

# solution 1: delete lefthand scenes not matching righthand, then vrt
# solution 2: create dummy scenes righthand, then vrt

# pair = c(files[[1]][4], files[[2]][4])

# load all in as raster::brick in a list
rasters = map(files, ~ map(.x, ~ brick(.x)))

command = sprintf("gdalbuildvrt -separate -overwrite %s %s %s", outputdir, pair[1], pair[2])
# command = sprintf("gdalinfo %s", files[[1]][1])

# gdalUtils::gdalbuildvrt(pair, outputdir, separate = T, overwrite = TRUE)

system(command)

# VISO -------------------------------------------------------------------------

ras1 = rasters[[1]][[4]]
ras2 = rasters[[2]][[4]]

plot(ras1[[2]])

head(names(ras1))
head(names(ras2))

ras1[[1:10]] %>% writeRaster("F:/geodata/geo402/S2/xx_S2_indices/mosaics/ras1.tif"
ras2[[1:10]] %>% writeRaster("F:/geodata/geo402/S2/xx_S2_indices/mosaics/ras2.tif")


-# TODO
#' only include dates, where dates match.
#' NOT: separate = TRUE
#'
#' check if the data really means the right UTM tile!
#'
#'
#' stack_35JMH_dvi kaputt, gehört to 35JNH!
