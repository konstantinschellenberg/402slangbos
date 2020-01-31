# Script to iteratively
# (1) search S2 scenes in 20m res and QA.
# (2) filter on those who have NA in most of the image
# (2) crop to region extent
# (3) apply QA mask on S2 -> masking
# (4) store as stacked file (brick)

par(mfrow=c(1,1))

source("import.R")

library(raster)
library(stars)
library(tidyverse)
if (require(pbapply)) { # install it, if FALSE
    pboptions(type = "timer")
}

list.reflectance_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "atm_20m.tif", recursive = T, full.names = TRUE)
list.cm_on_storage = list.files(path = "F:/geodata/geo402/s2/", pattern = "CM.tif$", recursive = T, full.names = TRUE)

list.reflectance = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2", pattern = "atm_20m.tif", recursive = T, full.names = TRUE)
list.cm = list.files(path = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm", pattern = "CM.tif$", recursive = T, full.names = TRUE)

# stack dirs
path_s2 = "D:/Geodaten/#Jupiter/GEO402/01_data/s2"
path_cm = "D:/Geodaten/#Jupiter/GEO402/01_data/s2/cm/"
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

# get one cm
cm = stack(list.cm)

# description of cm
cm@data@attributes[[1]]

# Clound Mask cleaning----------------------------------------------------------

cm.na = cellStats(is.na(cm), sum) # count the NA values in each layer
cm.na.fraction = cm.na/ncell(cm) # fraction that is NA
cm.na.fraction

cm.filtered.out = cm[[which(cm.na.fraction>0.2)]] # filter all with na more than 20%
names(cm.filtered.out) # return dir with NA rasters

# remove files with NA
file.remove(paste0(path_cm, names(cm.filtered.out), ".tif"))

# Reflectances cleaning --------------------------------------------------------

reflectances = brick(list.reflectance)
reflectances.na = cellStats(is.na(reflectances), sum)
reflectances.na.fraction = reflectances.na/ncell(reflectances)
reflectances.na.fraction

reflectances@data@attributes[[1]]

refl.filtered.out = cm[[which(reflectances.na.fraction>0.2)]] # filter all with na more than 20%
names(refl.filtered.out)

# remove files with NA
file.remove(paste0(path_cm, names(refl.filtered.out), ".tif"))

# Masking ----------------------------------------------------------------------
# mask function (for raster*)
make_mask = function(x){out = x == 2 | x == 3; return(out)}

# iterating
for (cm in list.cm){

    out.bulk = raster()

    # bulk create masks
    out = raster(x) %>%
        raster::calc(., fun = mask)
    out.bulk = addLayer(out)
    writeRaster(out.bulk, filename = paste0(path_cm, "all_masks.tif"))
}

for (re in list.reflectance)

raster::addLayer()

















# ------------------------------------------------------------------------------

gdal_options = list(bands = 2, 3, 4)

y = read_stars(a, proxy = TRUE)

y %>%
    slice("band", 2) %>%
    plot()

st_crs(ras1)

ras1 = stars::read_stars(paste0(path, a[[1]]), proxy = TRUE)
ras2 = stars::read_stars(paste0(path, a[[6]]), proxy = TRUE)

ras1 %>%
    slice("band", 2) %>%
    plot(hist = "hist")

plot(ras1)
plot(ras2)
