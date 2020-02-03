# Script to iteratively
# (1) search S2 scenes in 20m res and QA.
# (2) filter on those who have NA in most of the image
# (2) crop to region extent
# (3) apply QA mask on S2 -> masking
# (4) store as stacked file (brick)

par(mfrow=c(5,5))

source("import.R")

library(raster)
library(stars)
library(tidyverse)
if (require(pbapply)) { # install it, if FALSE
    pboptions(type = "timer")
}
library(doParallel)
library(foreach)
library(rgdal)

list.reflectance_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "atm_20m.tif", recursive = T, full.names = TRUE)
list.cm_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "CM.tif$", recursive = T, full.names = TRUE)

list.reflectance = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", pattern = "atm_20m.tif$", recursive = T, full.names = TRUE)
list.cm = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm", pattern = "CM.tif$", recursive = T, full.names = TRUE)

# stack dirs
dir.create(path_cm)
dir.exists(path_cm)

# file.copy(from = list.reflectance, to = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", recursive = TRUE)
# file.copy(from = list.cm, to = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm", recursive = TRUE)

# check if there is data here:
dir("D:/Geodaten/#Jupiter/GEO402/01_data/s2")

# NDVI Test --------------------------------------------------------------------

func.ndvi = function(x) (x[8] - x[4])/(x[8] + x[4])

s = brick(list.reflectance[2])
p = read_stars(list.reflectance[2], proxy = TRUE)



# tesing speed of the raster calculations in stars and raster
# stars
s2.ndvi = st_apply(p, c("x", "y"), func.ndvi)  #stars
system.time(write_stars(s2.ndvi, dsn = paste0(development_path, "/s2/ndvi3.tif")))

# raster
s2.ndvi = system.time(calc(s, func.ndvi, filename = paste0(development_path, "/s2/ndvi3.tif")))


# Applying cloud mask to 1 scene (then scopeable) -------------------------------
# Importing #################

# get all cm
cm = stack(list.cm)

# description of cm
cm@data@attributes[[1]]

# Clound Mask cleaning----------------------------------------------------------

cm.na = cellStats(is.na(cm), sum) # count the NA values in each layer
cm.na.fraction = cm.na/ncell(cm) # fraction that is NA
cm.na.fraction

cm.filtered.out = cm[[which(cm.na.fraction>0.2)]] # filter all with na more than 20%
names(cm.filtered.out) # return dir with NA rasters

# saveRDS(cm.filtered.out, paste0(path_developement, "rda\\cm_filtered.rds"))
# cm.filtered.out = readRDS(paste0(path_developement, "rda\\cm_filtered.rds"))

# remove files with NA
file.remove(paste0(path_cm, names(cm.filtered.out), ".tif"))

# ------------------------------------------------------------------------------
# parsing cm file names to eradicate also atm scene with NA > 20%

s2.filtered.out = cm.filtered.out %>% # creating vector of characters to delete S2 scenes
    names() %>%
    substring(first = 1, last = nchar(.[1]) - 3) %>%
    paste0("_atm_20m.tif")

s2.filtered.out
file.exists(paste0(path_s2, s2.filtered.out))
# file.remove(paste0(path_s2, s2.filtered.out))

############################ NA CLEANED ########################################
# Masking ----------------------------------------------------------------------

library(gdalUtils)


# BUILD VRT --------------------------------------------------------------------
## checkup
c(st_bbox(study_area)) # our bounding box
identical(length(list.cm), length(list.reflectance)) # same amount of layers in the folders == TRUE
##

# mask function (for raster*)


cores = detectCores() - 1
c1 = makeCluster(cores)
registerDoParallel(c1)
# ------------------------------------------------------------------------------
# loop 1 (cropping S2 and band selection (4 [red], 8 [infrared]), res: 20m)
for (i in seq_along(list.reflectance)){
    file.remove(path_vrt)

    gdalUtils::gdalbuildvrt(gdalfile = list.reflectance[i],
                            output.vrt = path_vrt,
                            te = c(st_bbox(study_area)),
                            overwrite = TRUE,
                            a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

    outfile.crop = substring(list.reflectance[i], first = 1, last = nchar(list.reflectance[i]) - 4) %>%
        paste0(., "_crop.tif")

    gdalUtils::gdal_translate(src_dataset = path_vrt,
                              dst_dataset = outfile.crop,
                              overwrite = TRUE,
                              b = c(4,8))

}
# ------------------------------------------------------------------------------
# loop 2 (cropping cloud mask)
for (i in seq_along(list.cm)){
    file.remove(path_vrt)

    # crop cloud masks to area extent
    gdalUtils::gdalbuildvrt(gdalfile = list.cm[i],
                            output.vrt = path_vrt,
                            te = c(st_bbox(study_area)),
                            overwrite = TRUE,
                            a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

    outfile.cm = substring(list.cm[i], first = 1, last = nchar(list.cm[i]) - 4) %>%
        paste0(., "_crop.tif")

    gdalUtils::gdal_translate(src_dataset = path_vrt,
                              dst_dataset = outfile.cm,
                              overwrite = TRUE)
}

list.reflectance.crop = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2",
                                   pattern = "crop.tif$", recursive = FALSE, full.names = TRUE)
list.cm.crop = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm",
                          pattern = "crop.tif$", recursive = F, full.names = TRUE)

# ------------------------------------------------------------------------------
# loop 3 (applying cloud mask to Sentinel)
for (i in seq_along(list.reflectance.crop)){

    # load rasters
    ras = brick(list.reflectance.crop[i])
    cm = raster(list.cm.crop[i]) %>%
        make_mask() # making mask

    outfile.mask = substring(list.reflectance[i], first = 1, last = nchar(list.reflectance[i]) - 4) %>%
        paste0(., "_crop_cm.tif")

    out = ras %>%
        mask(cm, maskvalue = 1, filename = outfile.mask, overwrite = TRUE)
}

stopCluster(c1)

# Stacking bands ---------------------------------------------------------------
list.stacking = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2",
                           pattern = "crop_cm.tif$", recursive = FALSE, full.names = TRUE)
single.vrt = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", pattern = "crop_cm.tif$", recursive = FALSE) %>%
    substring(., first = 1, last = nchar(.[i]) - 4) %>%
    paste0("D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\s2\\vrt\\", ., ".vrt")


plot(read_stars(list.stacking[1:10], proxy = T))

path_vrt2 = paste0(path_developement, "s2\\", "reflectance2.vrt") # vrt path
path_merged = paste0(path_s2, "red.tif")

bands = 1:2

for (a in bands){

    for (i in seq_along(list.stacking)){

        file.remove(path_vrt)

        gdalUtils::gdalbuildvrt(gdalfile = list.stacking[i],
                                output.vrt = path_vrt,
                                overwrite = TRUE)

        gdalUtils::gdal_translate(src_dataset = path_vrt,
                                  dst_dataset = single.vrt[i],
                                  overwrite = TRUE,
                                  b = a)

    }

    gdalUtils::gdalbuildvrt(gdalfile = single.vrt,
                            output.vrt = path_vrt,
                            overwrite = TRUE, separate = TRUE)

    gdalUtils::mosaic_rasters(gdalfile = path_vrt,
                              dst_dataset = path_merged,
                              separate = TRUE)

}

GDALinfo(path_vrt)
