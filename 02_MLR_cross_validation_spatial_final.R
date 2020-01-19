######################################################################
# Woody Cover Estimation using Sentinel-1 time series and Lidar data
# 
# This script belongs to the Publication: 
#   Urban et. al (2020): Woody Cover Classification in the Savanna Ecosystem 
#         of the Kruger National Park Using Sentinel-1 Time Series. Koedoe, X,X.
#
#
# This script was prepared by Dr. Marcel Urban (marcel.urban@uni-jena.de) and Patrick Schratz (p.schratz@lmu.de)
# 
# Step 2/4 
# Spatial Cross Validation
#
######################################################################

######################################################################
#Loading Libraries
######################################################################

library(mlr)
library(data.table)
library(parallelMap)
library(raster)

######################################################################

######################################################################
#Data Preparation
######################################################################

# Input of stack, which is containing training and refernce data
data_input<-brick("/XXX/XXX/XXX/S1_A_VH_VV_16_17_lidar")

#or import .rda file saved in 01_MLR_tune_spatial.R (if so, continue with line 75: "Preparation of Task")
#data1 = readRDS("/pvdata3/marcel/04_RF_woody_cover_KNP_v6/02_lidar_wcover_S1_15_17/01_data_input/S1_A_VH_VV_16_17_lidar.rda")

# Define NoData Values (e.g. -99) and loop through data stack
nodata = -99
for(i in 1:nlayers(data_input)){
  print(names(data_input)[i])
  data_input[[i]][data_input[[i]] == nodata] <- NA
}

# Create data frame
data <- as.data.frame(data_input, xy = TRUE)

# change variable names
setnames(data, c(
  "x", "y", "VH_20160102", "VH_20160114", "VH_20160126",
  "VH_20160207", "VH_20160302", "VH_20160314", "VH_20160326",
  "VH_20160407", "VH_20160419", "VH_20160501", "VH_20160513",
  "VH_20160525", "VH_20160606", "VH_20160630", "VH_20160712",
  "VH_20160724", "VH_20160805", "VH_20160817", "VH_20160829",
  "VH_20160910", "VH_20160922", "VH_20161004", "VH_20161016",
  "VH_20161028", "VH_20161109", "VH_20161121", "VH_20161203",
  "VH_20161215", "VH_20161227", "VH_20170108", "VH_20170120",
  "VH_20170201", "VH_20170213", "VH_20170225", "VH_20170309",
  "VH_20170321", "VH_20170402", "VH_20170414", "VH_20170426",
  "VV_20160102", "VV_20160114", "VV_20160126", "VV_20160207",
  "VV_20160302", "VV_20160314", "VV_20160326", "VV_20160407",
  "VV_20160419", "VV_20160501", "VV_20160513", "VV_20160525",
  "VV_20160606", "VV_20160630", "VV_20160712", "VV_20160724",
  "VV_20160805", "VV_20160817", "VV_20160910", "VV_20160922",
  "VV_20161004", "VV_20161016", "VV_20161028", "VV_20161109",
  "VV_20161121", "VV_20161203", "VV_20161215", "VV_20161227",
  "VV_20170108", "VV_20170120", "VV_20170201", "VV_20170213",
  "VV_20170225", "VV_20170309", "VV_20170321", "VV_20170402",
  "VV_20170414", "VV_20170426", "lidar"
))

# Remove NoData values
data1 <- na.omit(data)

######################################################################
#Preparation of Task
######################################################################

# Define a variable "coords", which stores the geographic coordinates
coords <- as.data.frame(data1[c("x", "y")])

# remove the x and y coords from the data frame as they should not be used as a
# feature, only for partitioning
data1$x <- NULL
data1$y <- NULL
regr.task <- makeRegrTask(
  id = "lidar", data = data1, target = "lidar",
  coordinates = coords
)

######################################################################
#Random Forest
######################################################################

# Create Learner with input of parameter set from tuning
lrn_rf <- makeLearner("regr.ranger", mtry = 1, num.trees = 300)

# show parameter
getHyperPars(lrn_rf)
getParamSet(lrn_rf)

# which parameter are tunable
filterParams(getParamSet(lrn_rf), tunable = TRUE)

######################################################################
#Spatial Cross Vaildation
######################################################################

# Define the outer reampling iterations
outer <- makeResampleDesc("SpRepCV", folds = 5, reps = 100) # add SpRepCV for spatial

# parallization of spatial cross validation (Note: mode="multicore" 
# for Unix, mode="socket" for Windows)
parallelStart(mode = "multicore", level = "mlr.resample", cpus = 5)

# Spatial Cross Validation
woody_cover_spcv <- mlr::resample(lrn_rf, regr.task, resampling = outer, show.info = TRUE, measures = setAggregation(rmse, test.mean))

parallelStop()

# Save the Spatial Cross Validation
saveRDS(woody_cover_spcv, "/.../.../S1_A_VH_VV_16_17_lidar_rf_cross_val_spatial_100rep.rda")

######################################################################
######################################################################
######################################################################