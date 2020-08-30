#' Create Bandname csv for band-date assignments within Slangbos project
#' Init: 29.08.2020, Konstantin Schellenberg
#'
#' We bring the bandnames in the tidy format of example: <ndvi.2016.04.27>

# LOAD PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(raster)
library(stringr)

options(max.print = 200)

source("D:/Geodaten/Master/projects/402slangbos/functions.R")

# USER INPUT -------------------------------------------------------------------

rasterdir = "F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/results/"
raster = "stack_35JMH_ndvi.img"
path = paste(rasterdir, raster, sep = "")

prefix = "ndvi"

# PROCESSING -------------------------------------------------------------------

# load raster
ras = brick(path)
phrase = names(ras) # get names

# Substring
phrase.datum = substr(phrase, start = 12, stop = 19)

# convert to POSTict (R-date) format
phrase.date = as.Date(phrase.datum, format = "%Y%m%d") %>%
    as.character() %>%
    stringr::str_replace_all("-", ".")

# prepend the prefix to date information
bandnames = map_chr(phrase.date, function(x) paste0(prefix, ".", x))

# SAVE AS CSV ------------------------------------------------------------------

write.csv(bandnames, file = paste0(rasterdir, paste0("bandnames_", prefix), ".txt"), row.names = FALSE)

# FUNCTION DEFINITION for use in R

bandnames = function(file, prefix){

    # load raster
    ras = brick(file)
    phrase = names(ras) # get names

    # Substring
    phrase.datum = substr(phrase, start = 12, stop = 19)

    # convert to POSTict (R-date) format
    phrase.date = as.Date(phrase.datum, format = "%Y%m%d") %>%
        as.character() %>%
        stringr::str_replace_all("-", ".")

    # prepend the prefix to date information
    bdnames = map_chr(phrase.date, function(x) paste0(prefix, ".", x))

    outfile = paste0(file, ".txt")
    write.csv(bdnames, file = outfile, row.names = FALSE)
}

# DEMO
# bandnames("F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/stack_35JMH_ndvi.img", "ndvi")

# end (not run)
