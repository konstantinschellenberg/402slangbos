#' Making ground truth (reference data) tables
#'

source("import.R")

# ------------------------------------------------------------------------------

dstdir = "D:/Geodaten/#Jupiter/GEO402/03_develop/ground_reference_files/"

gt_from_raster(gt, "Name", vv, dst = path_tables, prefix = "vv")
gt_from_raster(gt, "Name", vh, dst = path_tables, prefix = "vh")

gt_from_raster(gt, "Name", covv, dst = path_tables, prefix = "covv")
gt_from_raster(gt, "Name", covv_all, dst = path_tables, prefix = "covv_all")

gt_from_raster(gt, "Name", red, dst = path_tables, prefix = "red")
gt_from_raster(gt, "Name", nir, dst = path_tables, prefix = "nir")
