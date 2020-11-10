# extracting data in the format required by mlr³
# init 09.11.2020, K. Schellenberg

source("D:/Projects/402slangbos/import_rasters_classification.R")
source("D:/Projects/402slangbos/import_samples.R")

library(exactextractr)

# DATA IMPORT OF FULLY IMPUTED DATA --------------------------------------------


# TODO: repair, so that nodata areas have their own class!
gt_classif %>% as_tibble() %>% print()

# USER INPUT ################

basepath = "D:/Geodaten/GEO402/03_develop/classification"

start_dates = c("2015-10-01",
                "2017-10-01",
                "2019-10-01")
end_dates = c("2016-09-30",
              "2018-09-30",
              "2020-09-30")

tablenames = map(start_dates, function(start){

  p.tables = file.path(basepath, paste0("input_samples_", start, ".RDS"))
  #p.filledcubes = file.path(dir.stacks, paste0("datecube_", start, "_filled.envi"))
  return(p.tables)

})


# DATACUBE
# IMPORTANT: select a datacube preprocessed in datacube.R
cube = cubes[[2]]
file = tablenames[[2]]


# ANALYSIS ################
# TODO: Write as loop to automatise
# TODO: Make outer split

extraction = exact_extract(cube, gt_classif, include_xy = TRUE, max_cells_in_memory = 2e+10,
                  force_df = TRUE, full_colnames = TRUE, include_cols = "classif")

# layer names are important!

# bind rows together
# TODO: Push classes to the end
dt = rbindlist(extraction)

# exclude where coverage fraction < 0.5
cat("no. of observations covering pixels at all:", nrow(dt), sep = "\n")

# .SD = all columns, special character
dt.filter = dt[coverage_fraction > 0.5, .SD]
cat("no. of observations covering pixels by > 50%:", nrow(dt.filter), sep = "\n")

# sums the NAs
nas = dt.filter[, map(.SD, ~ sum(is.na(.x)))] %>% sum()
cat("total cells containing NAs", nas, sep = "\n")
cat("should be zero by now")

dt.na = na.omit(dt.filter)
cat("no. of observations surviving NA clearing", nrow(dt.na), sep = "\n")

# delete column "coverage_fraction"
dt.out = dt[, !c("coverage_fraction")]


# TODO: column "classif" must be factors
#input = input.in[ , classif:=as.factor(classif)]

# save as RDS file
write_rds(dt.out, file = file)

# (clearning end)

