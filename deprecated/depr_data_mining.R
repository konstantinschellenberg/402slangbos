# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to run the random forest classification.
#____________________________________________________________
# watch the video for introduction in RF for remote sensing
# https://www.youtube.com/watch?v=fal4Jj81uMA

# for tuning
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

source("import.R")

library(sf)
library(raster)
library(caret)

# Read in training data-----------------------------------------------------------

# time consuming task!
system.time(gt_from_raster(raster = raster_test, outfile = "_test"))

learning_input = readRDS(paste0(rds_path, "learning_input_VH.rds"))
learning_input_test = readRDS(paste0(rds_path, "learning_input_test.rds"))

gt_list = readRDS(paste0(rds_path, "gt_list_test.rds"))

if (!file.exists(paste0(rds_path, "learning_input.rds"))) {
    gt_from_raster(train_data = gt, response_col = "Name", raster = s1vv)
    sdf_all = readRDS(paste0(rds_path, "learning_input.rds"))
} else {sdf_all = readRDS(paste0(rds_path, "learning_input.rds"))}

# BAUSTELLE
# loop to get all input variables
# list_rasters = list(s1vv, s1vh) # and sentinel-2
# for i in list_rasters{
#     i = i[[seq(1, 3)]]
#     gt_from_raster(train_data = gt, response_col = "Name", raster = i, outfile = chr(i))
# }
# toString(i)

################## HERE
# ds_all returns nur eine Klasse.........
# subsetting

nsamples = 1000
sdf_all = df_all[sample(1:nrow(df_all), nsamples), ]

# train model
modFit_rf = train(as.factor(class) ~ X2015.03.11, method = "rf", data = sdf_all,
                   importance=TRUE,
                   trControl=trainControl(method="cv",number=5))

# predict model
beginCluster()
system.time(preds_rf = clusterR(raster, raster::predict, args = list(model = modFit_rf)))
endCluster()

# write as file
raster::writeRaster(preds_rf, "D:\\Geodaten\\#Jupiter\\GEO402\\04_products\\rf\\preds_rf_20200112_vh_sample10_CV.tif", overwrite=TRUE)

# spacetime
# blast
# CAST
# stars

