# Sentinel-1 cleaning

source("import.R")

# fill no-data `gdal` and cropping ---------------------------------------------

s1vv_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:/Geodaten/#Jupiter/GEO402/01_data/s1_data/S1_A_D_VH_free_state_study_area_geo402"


gdalUtils::gdalinfo(s1vh_path)
gdalUtils::gdalinfo(s1vv_path)

file.remove(path_vrt)

gdalUtils::gdalbuildvrt(gdalfile = s1vh_path,
                        output.vrt = path_vrt,
                        overwrite = TRUE,
                        te = st_bbox(study_area),
                        a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        vrtnodata = -99L)

gdalUtils::gdal_translate(src_dataset = path_vrt,
                          dst_dataset = paste0(path_s1, "S1_A_D_VH_free_state_study_area_geo402_fillna_crop"),
                          of = "ENVI")

gdalUtils::ca


# # write bandnames to txt s1
# v = names(vv) %>%
#     substr(start = 13, stop = 20) %>%
#     as.data.frame() %>%
#     write.csv(file = paste0(path_s1, "s1_bandnames_old.txt"), quote = FALSE, row.names = FALSE)
