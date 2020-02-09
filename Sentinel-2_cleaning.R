# Script to iteratively
# (1) search S2 scenes in 20m res and QA.
# (2) filter on those who have NA in most of the image
# (3) crop to region extent
# (4) apply QA mask on S2 -> masking
# (5) store as stacked file (brick)
# (6) make cloud free

par(mfrow=c(5,5))

source("import.R")

library(stars)
library(rgdal)
library(gdalUtils)


# (1) search S2 scenes in 20m res and QA. --------------------------------------
################################################################################

list.reflectance_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "atm_20m.tif", recursive = T, full.names = TRUE)
list.cm_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "CM.tif$", recursive = T, full.names = TRUE)

list.reflectance = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", pattern = "atm_20m.tif$", recursive = T, full.names = TRUE)
list.cm = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm", pattern = "CM.tif$", recursive = T, full.names = TRUE)

# stack dirs
# dir.create(path_cm)
# dir.exists(path_cm)


# copying to SSD disc:
# file.copy(from = list.reflectance, to = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", recursive = TRUE)
# file.copy(from = list.cm, to = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm", recursive = TRUE)

# check if there is data here:
dir("D:/Geodaten/#Jupiter/GEO402/01_data/s2") # what's in the directory?


# (2) filter NA ----------------------------------------------------------------
################################################################################

cm.na = cellStats(is.na(cm), sum) # count the NA values in each layer
cm.na.fraction = cm.na/ncell(cm) # fraction that is NA
cm.na.fraction

cm.filtered.out = cm[[which(cm.na.fraction>0.2)]] # filter all with na more than 20%
names(cm.filtered.out) # return dir with NA rasters

# saveRDS(cm.filtered.out, paste0(path_developement, "rda\\cm_filtered.rds"))
# cm.filtered.out = readRDS(paste0(path_developement, "rda\\cm_filtered.rds"))

# remove files with NA
# file.remove(paste0(path_cm, names(cm.filtered.out), ".tif"))

# parsing cm file names to eradicate also atm_20 scenes with NA > 20%

s2.filtered.out = cm.filtered.out %>% # creating vector of characters to delete S2 scenes
    names() %>%
    substring(first = 1, last = nchar(.[1]) - 3) %>%
    paste0("_atm_20m.tif")

s2.filtered.out
file.exists(paste0(path_s2, s2.filtered.out))
# file.remove(paste0(path_s2, s2.filtered.out))

# (3) crop to region extent ----------------------------------------------------
################################################################################

## checkup
c(st_bbox(study_area)) # our bounding box
identical(length(list.cm), length(list.reflectance)) # same amount of layers in the folders == TRUE

# parallel
cores = detectCores() - 1
c1 = makeCluster(cores)
registerDoParallel(c1)

# (3a) -------------------------------------------------------------------------
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

# (3b) -------------------------------------------------------------------------
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

# (4) Applying cloud mask to Sentinel -------------------------------------------------------------------------

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

# (5) Stacking bands -----------------------------------------------------------

list.stacking = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2",
                           pattern = "crop_cm.tif$", recursive = FALSE, full.names = TRUE)

single.vrt = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", pattern = "crop_cm.tif$", recursive = FALSE) %>%
    substring(., first = 1, last = nchar(.[1]) - 4) %>%
    paste0("D:\\Geodaten\\#Jupiter\\GEO402\\03_develop\\s2\\vrt\\", ., ".vrt")


plot(read_stars(list.stacking[1:10], proxy = T))

path_merged.red = paste0(path_s2, "red.tif")
path_merged.nir = paste0(path_s2, "nir.tif")
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
                              dst_dataset = if(a == 1){path_merged.red} else {path_merged.nir},
                              separate = TRUE)

}

# check info
GDALinfo(path_merged.red)

# ANNEX
# NDVI Test --------------------------------------------------------------------

func.ndvi = function(x) (x[8] - x[4])/(x[8] + x[4])

s = brick(list.reflectance[2])
p = read_stars(list.reflectance[2], proxy = TRUE)

# testing speed of the raster calculations in stars and raster
# stars
s2.ndvi = st_apply(p, c("x", "y"), func.ndvi)  #stars
system.time(write_stars(s2.ndvi, dsn = paste0(development_path, "/s2/ndvi3.tif")))

# raster
s2.ndvi = system.time(calc(s, func.ndvi, filename = paste0(development_path, "/s2/ndvi3.tif")))

# ------------------------------------------------------------------------------

# creating small extents
st_bbox(smaller_extent)

gdalUtils::gdalbuildvrt(gdalfile = s1vh_path,
                        output.vrt = path_vrt,
                        separate = FALSE,
                        overwrite = TRUE, te = st_bbox(smaller_extent))


gdalUtils::gdal_translate(src_dataset = path_vrt,
                          dst_dataset = paste0(path_s1, "vh_small.tif"),)
