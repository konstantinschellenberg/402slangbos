# Sentinel-1 cleaning

source("import.R")

rgdal::GDALinfo(s1vh_path)

# fill no-data `raster`---------------------------------------------------------
df = data.frame(id = NA, v=-30)

# doesn't work: . . .
# subs(vh, df, filename = paste0(path_s1, "S1_A_D_VH_free_state_study_area_geo402_filled.tif"))

mean(vh[[1]])

# fill no-data `gdal`-----------------------------------------------------------

gdalUtils::gdalinfo(s1vh_path)

gdalUtils::gdalbuildvrt(gdalfile = s1vh_path,
                        output.vrt = path_vrt,
                        overwrite = TRUE,
                        a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        srcnodata = -99,
                        vrtnodata = -10L)

gdalUtils::gdal_translate(src_dataset = path_vrt,
                          dst_dataset = paste0(path_s1, "S1_A_D_VH_free_state_study_area_geo402_fillna.tif"),
                          a_nodata = 0L)

