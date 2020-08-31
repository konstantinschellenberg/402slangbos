#' creating VRTs
#'

library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
source("D:/Geodaten/Master/projects/402slangbos/functions.R")


# USER INPUT -------------------------------------------------------------------

dirs = c("F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/",
         "F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JNH/")

# CREATE DATES -------------------------------------------------------------------

# get directories only of files ending with "img" (= raster information)
files = map(dirs, ~ list.files(.x, full.names = T, recursive = T, pattern = "img$"))

# create bandnames .txt
for (i in seq_along(files)){
    map(files[[i]], function(x) {
        split = str_split(x, "_")
        prefix = map_chr(split, ~ .x[length(.x)])
        prefix2 = map(prefix, ~ str_sub(.x, 1, -5))
        bandnames(x, prefix = prefix2, writeout = TRUE)
    })

}

# example for one raster
ras = files[[1]][[1]]
gdalUtils::gdalinfo(ras)
bandnames(ras, prefix = "new")

# show all date of the bands
map(files, ~ map(.x, ~ bandnames(.x)))

# SPLIT STACKS -----------------------------------------------------------------

# create single file for each layer (virtual VRT format)
map(files, ~ map(.x, function(ras){

    # get nlayers, iteration basis
    ras.in = brick(ras)
    nlay = ras.in %>% nlayers()
    bandnames = names(ras.in)

    dir = str_split(ras, "/")
    dir = dir[[1]][1:length(dir[[1]]) - 1] %>%
        paste(collapse = "/")

    split = str_split(ras, "_")
    prefix = map_chr(split, ~ .x[length(.x)]) %>%
        map(., ~ str_sub(.x, 1, -5))

    dir.out = paste(dir, prefix, sep = "/")
    if (!dir.exists(dir.out)){
        dir.create(dir.out)
    }

    for (i in 1:nlay){

        cat("no. ", i, "\n")
        band = i
        output = paste0(dir.out, "/", bandnames[i], ".vrt")

        command = sprintf("gdalbuildvrt -b %s %s %s", band, output, ras)
        system(command)
    }
}))


# PYROSAR ----------------------------------------------------------------------
# now switch to pyroSAR in python and execute groupbyTime on the stacks

# STACKING ---------------------------------------------------------------------

mosaics = "F:/geodata/geo402/S2/xx_S2_indices/mosaics/"
files = list.files(mosaics, full.names = T)

# exclude VRT data
files.mosaic = files[!grepl(files, pattern = ".vrt$")]

names.stack = str_split(files.mosaic, "/") %>%
    map_chr(., ~ .x[length(.x)])

for (i in seq_along(files.mosaic)){
    print(i)

    # concat outfile name
    name = names.stack[i]
    output = paste0(mosaics, name, ".vrt")

    stack.dir = paste0(files.mosaic[i], "/*.tif")
    # scenes = list.files(stack.dir, full.names = TRUE)

    command = sprintf("gdalbuildvrt -separate -srcnodata -9999 -vrtnodata -9999 -overwrite %s %s", output, stack.dir)
    system(command)
}
i = 1
# for xy



# TODO
# make everything nodata which is outside possible areas (NDVI >1 <-1 etc.)
