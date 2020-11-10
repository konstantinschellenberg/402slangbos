# Title     : Pipe through the datacube functions
# Objective : Create a raster*stack preserving metainformation
# Created by: Konsti
# Created on: 09.11.2020

cat("Loading rasters", sep = "\n")

source("D:/Projects/402slangbos/import_rasters_classification.R")

# TODO: save as netCDF Format, can supply additional arguments
# TODO: output textfile with layernames (or save them with the output!)

# USER INPUT ######################################

dir.stacks = "D:/Geodaten/GEO402/01_data/stack"
maske = "D:/Geodaten/GEO402/01_data/mask.tif"

start_dates = c("2015-10-01",
                "2017-10-01",
                "2019-10-01")
end_dates = c("2016-09-30",
              "2018-09-30",
              "2020-09-30")

cubenames = map(start_dates, function(start){
  p.cubes = file.path(dir.stacks, paste0("datecube_", start, ".envi"))
  p.filledcubes = file.path(dir.stacks, paste0("datecube_", start, "_filled.envi"))
  return(list(p.cubes, p.filledcubes))
})

# must be loaded via the import file sourced
# Data MUST be raster*bricks
rasters = list(co, ndvi, savi, vh)
names(rasters) = layernames

# 0.85 is the extent of SLC with path loss
thresh = 0.85 # share of valid pixels to keep

save = TRUE
overwrite = TRUE

start = start_dates[1]
#ende = end_dates[1]
#name = "co"

# Processing ######################################

# test of crs's are identical
crs = map(rasters, ~ crs(.x))
crs_true = map_lgl(crs, ~ identical(.x, crs[[1]]))

if (!all(crs_true)) stop("The CRS are not identical!")

stks = map2(start_dates, end_dates, function(start, ende){

  cat("Timespan\n")
  cat(start, ende, sep = " - ")
  cat("\n")

  raslist = map2(rasters, names(rasters), function(r, name){
    cat(sprintf("Performing fraction cleaning on %s ", name), sep = "\n")
    frac = fraction_cleaner_z(r, thresh, surpress_stdout = TRUE)

    cat("Querying by date", sep = "\n")
    time = sub_temporal_z(frac, start, ende)
    return(time)
  })

  cat("Stacking datasets", sep = "\n")
  stk = stacking_z(raslist)
  return(stk)

  cat("Data Cube Block created!")
})

if (save){

  map(start_dates, function(start){

    map2(stks, cubenames, function(stk, filename){

      file = filename[[1]]

      cat("Writing out datacube, will take very long!", sep = "\n")
      cat("Writing to:", file, sep = "\n")
      cat("Number of layers:", nlayers(stk), "\n")

      # speeds up saving action
      inmem = readAll(stk)
      writeRaster(inmem, filename = file, format = "ENVI", overwrite = overwrite)
      cat("Data Cube Block saved!")

    })
  })
}

cubenames[[1]][[1]]

# Postprocessing with gdal NA -> 0 (IMPUTATION SIMPLE)
  # fillna with gdal

map(cubenames, function(filenames){

  cube = filenames[[1]]
  outfile = filenames[[2]]

  expression = "where(B==0,0,A)"

  cmd = sprintf(paste0("python C:\\Programme\\GDAL\\gdal_calc.py --calc %s -A %s --allBands A -B %s",
                       " --B_band 1 --overwrite --format ENVI --outfile %s"), expression, cube, maske, outfile)
  system(cmd)
  print(cmd)
  return = NULL

})

# (end)