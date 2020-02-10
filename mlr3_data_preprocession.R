# Script for preprocession and hamonisation of S2 and S1 data
# Konstantin Schellenberg

# TRAINING
######################################################################
# Loading Libraries & refer to import source
######################################################################

source("import.R")
getOption("max.print")
options(max.print = 1000)


######################################################################
# Training Dataset
######################################################################

# create input tables

# full
x = 0
if (x == 1){
    file.remove(c(paste0(rds_path, "learning_input_vh.rds"),
                  paste0(rds_path, "learning_input_red.rds"),
                  paste0(rds_path, "learning_input_nir.rds")))
    gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
    gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
    gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")
}

# gt_from_raster(raster = vv, train_data = gt, response_col = "Name", outfile = "vv")
# # small
# gt_from_raster(raster = vh_small, train_data = gt_smaller, response_col = "Name", outfile = "vh_sm")
# gt_from_raster(raster = red_small, train_data = gt_smaller, response_col = "Name", outfile = "red_sm")
# gt_from_raster(raster = nir_small, train_data = gt_smaller, response_col = "Name", outfile = "nir_sm")


# load input tables

# full
vh_input = readRDS(paste0(rds_path, "learning_input_vh.rds"))
red_input = readRDS(paste0(rds_path, "learning_input_red.rds"))
nir_input = readRDS(paste0(rds_path, "learning_input_nir.rds"))

# vv_input = readRDS(paste0(rds_path, "learning_input_vv.rds"))
# # small
# vh_input = readRDS(paste0(rds_path, "learning_input_vh_sm.rds"))
# red_input = readRDS(paste0(rds_path, "learning_input_red_sm.rds"))
# nir_input = readRDS(paste0(rds_path, "learning_input_nir_sm.rds"))


# deleting previous:


# merge data.frames, find out with cols are dublicates, coords and class column only once

input = bind_task(vh_input, red_input, nir_input)

######################################################################
# Prediction Dataset
######################################################################

# data = as.data.table.raster(vh, xy = TRUE, inmem = F)
# 4 extent instances for the prediction to fit into memory

coords = st_bbox(study_area)
x = (coords$xmax - coords$xmin) / 2 + coords$xmin

xmax - ymax

for (tr in coords)

# warp to smaller extent (4 tiles in total)
gdalUtils::gdalbuildvrt(gdalfile = s2red,
                        output.vrt = path_vrt,
                        overwrite = TRUE)

gdalUtils::gdal_translate(src_dataset = path_vrt,
                          dst_dataset = single.vrt[i],
                          overwrite = TRUE,
                          tr = )


# red = as.data.table.raster(red_small, xy = TRUE)
# nir = as.data.table.raster(nir_small, xy = TRUE)

vh.in = as.data.table.raster(vh, xy = TRUE, inmem = FALSE)
vv.in = as.data.table.raster(vv, xy = TRUE, inmem = FALSE)

#################### FUNCTION HERE
# merge matrices, find out with cols are dublicates
dts3 = as_tibble(cbind(vh.in, vv.in), .name_repair = "unique")

# remove cols with x, y and class from the data frame, rename vars from the last binded data frame to x, y and class
# e.g. -vh_x, -vh_y, -class...356, -class...557, class = class...758, -red_x, -red_y, x = nir_x, y = nir_y
dts2 = dts3 %>%
    .[,3:(length(.))] %>%
    dplyr::select(-starts_with("x"), -starts_with("y")) %>%
    cbind(dts3[,1:2]) %>%
    dplyr::rename(x = starts_with("x"),
                  y = starts_with("y"))

# remove cols with NA (prerequisit for random forest input)
dts = dts2 %>%
    as.data.frame() %>%
    .[, colSums(is.na(.)) == 0] %>%
    as.data.table()

# number of variables:
length(names(dts))
class(dts)
