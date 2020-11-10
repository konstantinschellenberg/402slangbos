# Title     : Creating tiles or datacube to be funnelled to model prediction
# Created by: Konsti
# Created on: 10.11.2020

# extracting data in the format required by mlr³
# init 09.11.2020, K. Schellenberg

source("D:/Projects/402slangbos/import_rasters_classification.R")
source("D:/Projects/402slangbos/import_samples.R")

library(exactextractr)

# USER INPUT ################################################################

basepath = "D:/Geodaten/GEO402/03_develop/classification"

start_dates = c("2015-10-01",
                "2017-10-01",
                "2019-10-01")
end_dates = c("2016-09-30",
              "2018-09-30",
              "2020-09-30")

tablenames = map(start_dates, function(start){

  p.tables = file.path(basepath, paste0("predictionset_", start, ".RDS"))
  #p.filledcubes = file.path(dir.stacks, paste0("datecube_", start, "_filled.envi"))
  return(p.tables)

})

cube = cubes[[2]]
file = tablenames[[2]]

# must be 0 -> dimension identical! Mind the additional three columns in input_table (x, y, classif)
((dim(input_table)[2] - 3) - dim(cube)[3]) == 0


# ANALYSIS ##################################################################

dt = as.data.table.raster(cube, row.names = NULL, xy = TRUE)
#raster::canProcessInMemory(cube, 1)

# exclude where coverage fraction < 0.5
cat("no. of observations covering pixels at all:", nrow(dt), sep = "\n")

# sums the NAs
nas = dt[, map(.SD, ~ sum(is.na(.x)))] %>% sum()
cat("total cells containing NAs", nas, sep = "\n")
cat("should be zero by now")

# save as RDS file
write_rds(dt, file = file)

# (cube coerction end)
