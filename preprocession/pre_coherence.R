# stacking Sentinel-1 coherences
# 2 weeks temporal sampling

source("import.R")

################################################################################
# Follow this for single-file stacking
################################################################################

# list coherence files

list.stacking = list.files(path = "F:/geodata/geo402/S1-coherence/xx_old_stack/raw_data", pattern = ".tif$", full.names = T)
coherence.names = list.files(path = "F:/geodata/geo402/S1-coherence/xx_old_stack/raw_data", pattern = ".tif$", full.names = F)

# for new coherences (including 2020 data)
coherence.names = names(brick("F:/geodata/geo402/S1-coherence/xx_new_stack/S1_A_VV_stack_coherence"))

single.vrt = coherence.names %>%
    substring(., first = 1, last = nchar(.[1]) - 4) %>%
    paste0("D:/Geodaten/#Jupiter/GEO402/03_develop/coherence/vrt/", ., ".vrt")

# (1) stack and crop -----------------------------------------------------------

path_merged.covh = paste0(path_coherence, "covh_nulls")
path_merged.covv = paste0(path_coherence, "covv_nulls")
bands = 1:2
a = 1
i = 1

# NA are 0 in the output coherences
for (a in bands){

    for (i in seq_along(list.stacking)){
        print(i)
        file.remove(path_vrt_co)
        file.remove(single.vrt[i])

        # get single bands (many vrts)
        # fetch one band according to the initial for-loop (VH and VV polarisation coherences, save as unique vrt (overridden by 2nd loop round))
        gdalUtils::gdal_translate(src_dataset = list.stacking[i],
                                  dst_dataset = single.vrt[i],
                                  overwrite = TRUE,
                                  of = "VRT",
                                  b = a)
            }

    file.remove(path_vrt_co)

    # summarise bands to virtual file (vrt)
    gdalUtils::gdalbuildvrt(gdalfile = single.vrt,
                            output.vrt = path_vrt_co,
                            overwrite = TRUE,
                            separate = TRUE,
                            srcnodata = 0) # here: decide to keep 0 or replace by NA

    # crop each date to study area extent, reproject to UTM 35S
    gdalUtils::gdalwarp(srcfile = path_vrt_co,
                        dstfile = if(a == 1){path_merged.covh} else {path_merged.covv},
                        of = "ENVI",
                        s_srs = "+proj=longlat +datum=WGS84 +no_defs",
                        t_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        te = c(st_bbox(study_area)),
                        tr = c(20, 20),
                        overwrite = TRUE)

}

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

