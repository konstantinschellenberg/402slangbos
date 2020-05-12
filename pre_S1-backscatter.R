# Sentinel-1 cleaning

source("import.R")

# create bandnames file as text

names = names(vv) %>%
    substr(start = 13, stop = 20) %>%
    as.numeric()

write.csv(names, file = paste0(path_s1, "xx_new_stack_backscatter_bandnames_30.txt"), row.names = FALSE)


# Pre-processing ---------------------------------------------------------------
#' (1) cut extent to study area
#' (2) leave no-data as -99
#' (3) identify scenes with significant -99 ocurrence

gdalUtils::gdalinfo(s1vv_path_50)
gdalUtils::gdalinfo(s1vh_path_30)

file.remove(path_vrt)

# (1) & (2)
# repeat this for VV and VH

gdalUtils::gdalbuildvrt(gdalfile = s1vh_path_50,
                        output.vrt = "D:/Geodaten/#Jupiter/GEO402/03_develop/s1/vrtpath.vrt",
                        overwrite = TRUE,
                        te = st_bbox(study_area),
                        a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
                        # vrtnodata = -99L)

gdalUtils::gdal_translate(src_dataset = "D:/Geodaten/#Jupiter/GEO402/03_develop/s1/vrtpath.vrt",
                          dst_dataset = paste0(path_s1, "S1_A_VH_stack_slangbos_50m_crop"),
                          of = "ENVI")

# (3)
# assess NA qualities
na_identifier(vv, na = -99)
na_identifier(vh,, na = -99)


