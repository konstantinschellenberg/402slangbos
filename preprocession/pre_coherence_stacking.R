# stacking Sentinel-1 coherences
# 2 weeks temporal sampling
# edit: 02.09.2020, Konstantin Schellenberg

source("import.R")

library(raster)
library(sf)
library(tidyverse)

################################################################################
# Follow this for single-file stacking
################################################################################

# list coherence files

list.stacking = list.files(path = "F:/geodata/geo402/S1_SLC/raw/S1A_IW_SLC_VV", pattern = ".tif$", full.names = TRUE)
coherence.names = list.files(path = "F:/geodata/geo402/S1_SLC/raw/S1A_IW_SLC_VV", pattern = ".tif$", full.names = FALSE)

single.vrt = coherence.names %>%
    substring(., first = 1, last = nchar(.[1]) - 4) %>%
    paste0("F:/geodata/geo402/S1_SLC/xx_new/VRTs/", ., ".vrt")

crop_area = st_read("F:/geodata/geo402/##study_area/LADYBRAND_final_enlarged_study_area.shp")
resolution = c(30, 30)

# crop_area to 32735
crop_area.utm = st_transform(crop_area, 32735)
utm_bbox = st_bbox(crop_area.utm)

# (1) stack and crop -----------------------------------------------------------

path_vrt = "F:/geodata/geo402/S1_SLC/xx_new/temp.vrt"
path_merged = "F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack.img"
bands = 1
a = 1
i = 1

# NA are 0 in the output coherences
for (i in seq_along(list.stacking)){
    print(i)
    file.remove(path_vrt)
    file.remove(single.vrt[i])

    # get single bands (many vrts)
    # fetch one band according to the initial for-loop (VH and VV polarisation coherences, save as unique vrt (overridden by 2nd loop round))
    gdalUtils::gdal_translate(src_dataset = list.stacking[i],
                              dst_dataset = single.vrt[i],
                              overwrite = TRUE,
                              of = "VRT",
                              b = a)
        }

file.remove(path_vrt)

# summarise bands to virtual file (vrt)
gdalUtils::gdalbuildvrt(gdalfile = single.vrt,
                        output.vrt = path_vrt,
                        overwrite = TRUE,
                        separate = TRUE,
                        srcnodata = 0) # here: decide to keep 0 or replace by NA

# crop each date to study area extent, reproject to UTM 35S
gdalUtils::gdalwarp(srcfile = path_vrt,
                    dstfile = path_merged,
                    of = "ENVI",
                    s_srs = "+proj=longlat +datum=WGS84 +no_defs",
                    t_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                    te = st_bbox(crop_area),
                    tr = resolution,
                    te_srs = "EPSG:4326",
                    overwrite = TRUE)

################################################################################
# Follow this for stack cleaning
################################################################################

# Pre-processing ---------------------------------------------------------------
#' (1) cut extent to study area
#' (2) leave no-data as -99
#' (3) identify scenes with significant -99 ocurrence
#' (4) create bandnames file
#' (5) remove 24d repeat cycle data

gdalUtils::gdalinfo(s1covv_all)
gdalUtils::gdalinfo(s1covv)
gdalinfo("D:/Geodaten/#Jupiter/GEO402/03_develop/s1/vrtpath.vrt")

nlayers(covv_all)
nlayers(covv)

# (1) & (2)
# repeat this for VV and VH
file.remove("D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/vrtpath.vrt")
gdalUtils::gdalbuildvrt(gdalfile = s1covv_all,
                        output.vrt = "D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/vrtpath.vrt",
                        overwrite = TRUE,
                        te = st_bbox(study_area),
                        a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", srcnodata = 0)

gdalUtils::gdal_translate(src_dataset = "D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/vrtpath.vrt",
                          dst_dataset = paste0(path_s1, "S1_A_VH_stack_slangbos_50m_crop"),
                          of = "ENVI")

# (3)
# assess NA qualities
na_identifier(covv, na = 0)
na_identifier(covv_all, na = NA)


# (4) create bandnames file txt ------------------------------------------------

# creating a textfile for automatic renaming of the raster bands according to the date
coherence.names

dates = substring(coherence.names, first = 10, last = 17) %>% as.numeric()
# write.csv(dates, file = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/xx_new_stack_coherence_bandnames.txt", row.names = F)

# (5) checking interferogram interval spacing --------------------------------------

dates = substring(coherence.names, first = 10, last = 17)
d = as.POSIXlt(x = dates, tryFormats = "%Y%m%d", tz = "GMT")

df = data.frame(date = d, diff = NA)
for (i in seq_along(d)) {
    # print(i)
    diff = as.numeric(difftime(time1 = d[[i+1]], time2 = d[[i]]))
    df$diff[[i]] = diff
}
df
# Interferograms with temporal baseline larger than 12d
filt_df = df %>%
    filter(diff > 12)
filt_df
pattern = filt_df$date %>% as.character() %>% gsub("-", ".", .)

#' Datum bezieht sich auf das erste SLC.
#' 8 Daten sind 24d-Paare
#'
raster = covv_all
falsepairs = grepl(pattern = paste(pattern, collapse = "|"), x = names(raster))

names_truepairs = names(raster)[!falsepairs]
out = raster[[names_truepairs]]
raster::writeRaster(out, filename = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/S1_A_VV_stack_coherence_reproj_resamp_rm24-repeat.tif")

# filename covv_all "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/S1_A_VV_stack_coherence_all_reproj_resamp_nodatafix_rm24-repeat.tif"
# filename covv     "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/S1_A_VV_stack_coherence_reproj_resamp_rm24-repeat.tif"

# (6) create bandnames for new coherence stacks (24-repeat cycle removed) ------

dates_renaming = names(out) %>%
    substring(first = 10, last = length(.)) %>%
    as.POSIXct(., tz = "GMT", tryFormats = "%Y.%m.%d") %>%
    gsub("-", "", .) %>%
    as.numeric()

# write.csv(dates_renaming, file = "D:/Geodaten/#Jupiter/GEO402/01_data/coherence/xx_new_stack_coherence_bandnames_all_rm24-repeat.txt", row.names = F, quote = F)


command = "gdalbuildvrt -r nearest -srcnodata -99 -tr 30 30 -te 463563.4 6739018.8 549706.4 6791364.4 -input_file_list
F:/geodata/geo402/S1_GRD/xx_new/S1_A_D_VV_stack_base_period_03_2015_03_2018_free_state_tile_1
F:/geodata/geo402/S1_GRD/xx_new/S1_A_D_VV_stack_base_period_03_2015_03_2018_free_state_tile_2
F:/geodata/geo402/S1_GRD/xx_new/S1A_GRD_VV_stack.vrt"
system(command)
