# stacking Sentinel-1 coherences
# 2 weeks temporal sampling

source("import.R")

library(rgdal)
library(gdalUtils)

# list coherence files

list.stacking = list.files(path = "F:/geodata/geo402/coherence/", pattern = ".tif$", full.names = T)
coherence.names = list.files(path = "F:/geodata/geo402/coherence/", pattern = ".tif$", full.names = F)

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


# (2) filter NA ----------------------------------------------------------------
# bands 14-24
# 166 in total


covh = brick(s1vh_co)
covv = brick(s1vv_co)

length(names(covh))
length(names(covv))

co.na = cellStats(covh == 0, sum) # count the NA (0) values in each layer
co.na.fraction = co.na/ncell(covh) # fraction that is NA (0)
co.na.fraction

co.filtered.out = covh[[which(co.na.fraction>0.1)]] # filter all with na more than 20%
names(co.filtered.out) # return dir with NA rasters

# (3) create bandnames file txt ------------------------------------------------

# creating a textfile for automatic renaming of the raster bands according to the date
coherence.names

co.dates = substring(coherence.names, first = 10, last = 17) %>% as.numeric()
write.csv(co.dates, file = path_naming_co, row.names = F)

# remove first row manually from the file!
