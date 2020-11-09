# extracting data in the format required by mlr³
# init 09.11.2020, K. Schellenberg

source("D:/Projects/402slangbos/import_rasters_classification.R")
source("D:/Projects/402slangbos/import_samples.R")

library(exactextractr)

# DATA IMPORT OF FULLY IMPUTED DATA --------------------------------------------

# IMPORTANT: select a datacube preprocessed in datacube.R
# DATACUBE
stk = stack(vh, ndvi) %>% brick()

classif %>% as_tibble() %>% print()

# ------------------------------------------------------------------------------

# inspect old version of the training dataset:
old_ds = readRDS("D:/Geodaten/GEO402/03_develop/rda/input.rds")

old_ds = as.data.table(readRDS("D:/Geodaten/GEO402/03_develop/rda/input.rds")) %>%
    dplyr::select(contains(c("vh", "red", "nir")), "x", "y", "class") %>%
    dplyr::select(sort(names(.)))



extraction = exact_extract(stk, classif, include_xy = TRUE, max_cells_in_memory = 2e+10,
                  force_df = TRUE, full_colnames = TRUE)

# layer names are important!

# bind rows together
dt = rbindlist(extraction)

# exclude where coverage fraction < 0.5
cat("no. of observations covering pixels at all:", nrow(dt), sep = "\n")

# .SD = all columns, special character
dt.filter = dt[coverage_fraction > 0.5, .SD]
cat("no. of observations covering pixels by > 50%:", nrow(dt.filter), sep = "\n")

# sums the NAs
nas = dt.filter[, map(.SD, ~ sum(is.na(.x)))] %>% sum()
cat("total cells containing NAs", nas, sep = "\n")

dt.na = na.omit(dt.filter)
cat("no. of observations surviving NA clearing", nrow(dt.na), sep = "\n")

# (clearning end)

