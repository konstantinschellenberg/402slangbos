# loading or generating the training data set of Ladybrand

source("import.R")

######################################################################
# Training Dataset
######################################################################

# create input tables

# full
x = 0
if (x == 1){

    # remove files
    file.remove(c(paste0(path_rds, "learning_input_vh.rds"),
                  paste0(path_rds, "learning_input_red.rds"),
                  paste0(path_rds, "learning_input_nir.rds"),
                  paste0(path_rds, "learning_input_covh.rds"),
                  paste0(path_rds, "learning_input_covv.rds")))

    # make new ground truth tables
    gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
    gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
    gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")
    gt_from_raster(raster = covh, train_data = gt, response_col = "Name", outfile = "covh")
    gt_from_raster(raster = covv, train_data = gt, response_col = "Name", outfile = "covv")

    # read in
    vh_input = readRDS(paste0(path_rds, "learning_input_vh.rds"))
    red_input = readRDS(paste0(path_rds, "learning_input_red.rds"))
    nir_input = readRDS(paste0(path_rds, "learning_input_nir.rds"))
    covh_input = readRDS(paste0(path_rds, "learning_input_covh.rds"))
    covv_input = readRDS(paste0(path_rds, "learning_input_covv.rds"))

} else {

    # read in
    vh_input = readRDS(paste0(path_rds, "learning_input_vh.rds"))
    red_input = readRDS(paste0(path_rds, "learning_input_red.rds"))
    nir_input = readRDS(paste0(path_rds, "learning_input_nir.rds"))
    covh_input = readRDS(paste0(path_rds, "learning_input_covh.rds"))
    covv_input = readRDS(paste0(path_rds, "learning_input_covv.rds"))
    covh_all_input = readRDS(paste0(path_rds, "learning_input_covh_all.rds"))
    covv_all_input = readRDS(paste0(path_rds, "learning_input_covv_all.rds"))

}

# coherences
# gt_from_raster(raster = covh, train_data = gt, response_col = "Name", outfile = "covh")
# gt_from_raster(raster = covv, train_data = gt, response_col = "Name", outfile = "covv")
gt_from_raster(raster = covh_all, train_data = gt, response_col = "Name", outfile = "covh_all")
gt_from_raster(raster = covv_all, train_data = gt, response_col = "Name", outfile = "covv_all")


nrow(vh_input)
nrow(red_input)
nrow(nir_input)
nrow(covh_input)
nrow(covv_input)
nrow(covh_all_input)
nrow(covv_all_input)

list = list(vh_input, red_input, covh_input)

# merge data.frames, find out with cols are dublicates, coords and class column only once
input = bind_task(list)

# write to disk
write_rds(input, path = paste0(path_developement, "rda/input.rds"))

# read in
input = readRDS(paste0(path_developement, "rda/input.rds"))
