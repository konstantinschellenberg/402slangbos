######################################################################
# Subtle features in slangbos encroachment, SA
#
# based on script by Urban, M. and Schratz, P.
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de),
# Patrick Schratz (p.schratz@lmu.de) and Konstantin Schellenberg (konstantin.schellenberg@posteo.de)
#
# Step 1/4
# Loading and tuning
#

######################################################################
# Loading Libraries & refer to import source
######################################################################

library(mlr)
library(data.table)
library(parallelMap)
library(raster)

source("import.R")

######################################################################
# Data Preparation
######################################################################

# Input of stack, which is containing training and reference data
data_input = readRDS(paste0(rds_path, "learning_input_test.rds"))
data_input = na.omit(data_input) # if there are NAs, remove them, potential error source
data_input$class = as_factor(data_input$class) # assign class as factor, not numeric

s1vv_re

# extract coordinates ##BAUSTELLE
new = raster(dataSet[[1]])
coord_col = as.data.frame(coordinates(new))

cbind.data.frame(dataSet[[1]], coord_col)


######################################################################
# Make Task
######################################################################

classif.task <- makeClassifTask(
    id = "slangbos", data = data_input, target = "class"
)

######################################################################
# Make Learner
######################################################################

# Classification tree, set it up for predicting probabilities
classif.lrn = makeLearner("classif.randomForest", predict.type = "response",
                          fix.factors.prediction = TRUE)

######################################################################
# Spatial Tuning
######################################################################

# show default parameters
getParamSet(classif.lrn)

# show which parameters are tunable
filterParams(getParamSet(classif.lrn), tunable = TRUE)
# --------------------------------------------------------------------

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

parallelStart(mode = "socket", level = "mlr.tuneParams", cpus = 5)

# Tuning the Random Forest
tune_rf <- tuneParams(classif.lrn,
                      task = classif.task, resampling = inner, par.set = ps,
                      control = ctrl, show.info = TRUE, measures = setAggregation(rmse, test.mean)
)

parallelStop()

# (Optional) Save the Tuned Random Forest
# saveRDS(tune_rf, "/.../.../rf_tuneGrid_results_mtry_1_2_ntrees_10_50_100_300_700_S1_16_17.rda")

##### TUNING END

######################################################################
#Spatial Cross Vaildation
######################################################################

# Define the outer reampling iterations
outer <- makeResampleDesc("SpRepCV", folds = 5, reps = 100) # add SpRepCV for spatial

# parallization of spatial cross validation (Note: mode="multicore"
# for Unix, mode="socket" for Windows)
parallelStart(mode = "multicore", level = "mlr.resample", cpus = 5)

# Spatial Cross Validation
woody_cover_spcv <- mlr::resample(classif.lrn, regr.task, resampling = outer, show.info = TRUE, measures = setAggregation(rmse, test.mean))

parallelStop()

# Save the Spatial Cross Validation
saveRDS(woody_cover_spcv, "/.../.../S1_A_VH_VV_16_17_lidar_rf_cross_val_spatial_100rep.rda")

######################################################################
######################################################################
######################################################################
