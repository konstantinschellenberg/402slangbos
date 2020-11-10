#' trying the package `gapfill` on a selected NDVI dataset as pre-
#' condition for modelling soundly
#' init: 08.11.2020, Konstantin Schellenberg

cat("Loading rasters", seq = "\n")
source("D:/Projects/402slangbos/import_rasters_classification.R")

# ----------------------------------------------------
# Load data

subs = st_read("D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "subset")
stacks = "D:/Geodaten/GEO402/01_data/stack/datacube"
filled_stacks = "D:/Geodaten/GEO402/01_data/stack/datacube_filled"
ext = raster::extent(subs)

a = fraction_cleaner(vh, thresh = 0.8)
getZ(a)

map(rasters, ~ crs(.x))

################################################################################
# (OPTIONAL) SPATIAL SUBSET ----------------------------------------------------

#cat("Cropping rasters to test subset")
#rasters = map(rasters, ~ crop(.x, ext))

################################################################################
# FILTER BY 80% general observation coverage -----------------------------------

cat("Performing fraction cleaning", seq = "\n")

# threshold indicates healthy pixels

rasters_f = map(rasters, ~ fraction_cleaner_z(.x, thresh = 0.9))


################################################################################
# TEMPORAL SUBSET --------------------------------------------------------------
# Between 01-10-2017 & 31-09-2018

#start_date = as.Date("2015-10-01")
#end_date = as.Date("2016-09-30")
#
start_date = as.Date("2019-10-01")
end_date = as.Date("2020-09-30")

#start_date = as.Date("2017-10-01")
#end_date = as.Date("2018-09-30")

# processing
cat("Querying by date", seq = "\n")

rasters_ft = map(rasters_f, ~ sub_temporal_z(.x, start_date, end_date))

################################################################################
# STACK FILE TO DATACUBE -------------------------------------------------------


rasters_fts = stacking_z(rasters_ft)

cat("Writing out datacube, will take very long!", seq = "\n")
writeRaster(rasters_tfs, filename = stacks, format = "ENVI", overwrite = TRUE)

################################################################################
# APPLY GAPFILLING ALGORITHM ---------------------------------------------------
#' (1) Applying constant NA value
#' (2) "intelligent" gap filling model
#' (3) Kriging interpolation
################################################################################
#' (1) Applying constant NA value
#' this takes forever...

# filling with 0

#rasters_fts[is.na(rasters_fts)] = 0

# write out
#writeRaster(rasters_fts, filename = filled_stacks, format = "ENVI", overwrite = TRUE)

#' (2a) "intelligent" gap filling model from `gapfill` -package
#' Takes extremely long!!
#library(gapfill)
#
#arr = array(rasters_fts, dim = c(241, 230, 33, 1))
#dim(arr)
#
#if(require(doParallel)){
#    registerDoParallel(7)
#    arr.filled = gapfill::Gapfill(arr, clipRange = c(0, 1))
#}
#
#arr.filled$fill # inspect array
#arr.filled$time
#
#Image(arr.filled$fill)


#' (2b) `Raster`s own approxNA functions relying on stats functions
#out = raster::approxNA(rasters_fts[[1:3]], method = "constant")

#' (3) Kriging interpolation
#' 1. Convert to Spatial Point grid
#' 2. perform kriging -> computation intensive!