######################################################################
# Woody Cover Estimation using Sentinel-1 time series and Lidar data
#
# This script belongs to the Publication:
#   Urban et. al (2020): Woody Cover Classification in the Savanna Ecosystem
#         of the Kruger National Park Using Sentinel-1 Time Series. Koedoe, X,X.
#
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de) and
# Patrick Schratz (p.schratz@lmu.de)
#
# Step 1/4
# Tuning
#
######################################################################

######################################################################
# Loading Libraries
######################################################################

library(mlr)
library(data.table)
library(parallelMap)
library(raster)

######################################################################

######################################################################
# Data Preparation
######################################################################

# Input of stack, which is containing training and refernce data
data_input<-brick("/.../.../S1_A_VH_VV_16_17_lidar")

#or import .rda file saved in 01_MLR_tune_spatial.R (if so, continue with line 50: "Preparation of Task")
#data1 = readRDS("/../../S1_A_VH_VV_16_17_lidar.rda")

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

# (Optional) Save data frame into file
# saveRDS(data1, "/.../.../S1_A_VH_VV_16_17_lidar.rda")

# INFO: You can read the saved file with the following command: 
# data1 = readRDS("/.../.../S1_A_VH_VV_15_17_lidar.rda")

######################################################################
# Task creation
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
# Random Forest
######################################################################

# Create Learner
lrn_rf <- makeLearner("regr.ranger")

# show default parameters
getParamSet(lrn_rf)

# show which parameters are tunable
filterParams(getParamSet(lrn_rf), tunable = TRUE)

######################################################################
# Spatial Tuning
######################################################################

# Defining parameter set
ps <- makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = 4),
  makeDiscreteParam("num.trees", values = c(10,50,100,300,700))
  # for random selection of num.trees use: makeIntegerParam("num.trees", lower = 10, upper = 700)
)

# Define the inner reampling iterations
ctrl <- makeTuneControlGrid() # for random selection of parameters use: ctrl <- makeTuneControlRandom(maxit = 100)
inner <- makeResampleDesc("SpCV", iters = 5)

# parallization of tuning (Note: mode="multicore" for Unix, mode="socket" for
# Windows)
parallelStart(mode = "multicore", level = "mlr.tuneParams", cpus = 5)

# Tuning the Random Forest
tune_rf <- tuneParams(lrn_rf,
  task = regr.task, resampling = inner, par.set = ps,
  control = ctrl, show.info = TRUE, measures = setAggregation(rmse, test.mean)
)

parallelStop()

# (Optional) Save the Tuned Random Forest
# saveRDS(tune_rf, "/.../.../rf_tuneGrid_results_mtry_1_2_ntrees_10_50_100_300_700_S1_16_17.rda")

##### TUNING END
