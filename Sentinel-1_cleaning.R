# Sentinel-1 cleaning

source("import.R")

# fill no-data `gdal`-----------------------------------------------------------

gdalUtils::gdalinfo(s1vh_path)

gdalUtils::gdalbuildvrt(gdalfile = s1vh_path,
                        output.vrt = path_vrt,
                        overwrite = TRUE,
                        te = st_bbox(study_area),
                        a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        srcnodata = -99,
                        vrtnodata = -10L)

gdalUtils::gdal_translate(src_dataset = path_vrt,
                          dst_dataset = paste0(path_s1, "S1_A_D_VH_free_state_study_area_geo402_fillna.tif"),
                          a_nodata = 0L)

# write bandnames to txt s1
v = names(olds1)[-c(14, 17, 62)] %>%
    substr(start = 13, stop = 20) %>%
    as.data.frame() %>%
    write.csv(file = paste0(path_s1, "s1_bandnames.txt"), quote = FALSE, row.names = FALSE)
