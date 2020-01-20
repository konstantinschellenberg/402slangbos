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

source("import.R")


######################################################################
# Data Preparation
######################################################################

# Input of stack, which is containing training and reference data
if (!file.exists(paste0(rds_path, "learning_input_try.rds"))) {
    print("file does not exist")
    gt_from_raster(raster = raster_test, outfile = "try")
    data_input = readRDS(paste0(rds_path, "learning_input_try.rds"))
} else {data_input = readRDS(paste0(rds_path, "learning_input_try.rds"))}

######################################################################
# Make Task
######################################################################

coords = as.data.frame(data_input[c("x", "y")])
data_input = dplyr::select(data_input, -x, -y) # removes coordinates from variable settings

classif.task = makeClassifTask(
    id = "slangbos", data = data_input, target = "class",
    coordinates = coords
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
ps = makeParamSet(
    makeIntegerParam("mtry", lower = 1, upper = 4),
    makeDiscreteParam("ntree", values = c(0, 10, 50, 100, 300, 500, 700))
    # for random selection of num.trees use: makeIntegerParam("num.trees", lower = 10, upper = 700)
)

# Define the inner reampling iterations
ctrl = makeTuneControlGrid() # for random selection of parameters use: ctrl = makeTuneControlRandom(maxit = 100)
inner = makeResampleDesc("SpCV", iters = 10L)
wrapper = makeTuneWrapper(classif.lrn, resampling = inner, par.set = ps,
                          control = ctrl, show.info = TRUE,
                          measures = list(acc, mmce))

# parallization of tuning (Note: mode="multicore" for Unix, mode="socket" for
# Windows)

if (!file.exists(paste0(rds_path, "tune_rf"))){

    # Tuning the Random Forest
    parallelStart(mode = "socket", level = "mlr.tuneParams", cpus = 5, mc.set.seed = TRUE)

    set.seed(27)
    tune_rf = tuneParams(learner = classif.lrn,
                          task = classif.task, resampling = inner, par.set = ps,
                          control = ctrl, show.info = TRUE, measures = list(acc, mmce)) # acc = accuracy, mmce = Mean misclassification error

    parallelStop()

    saveRDS(tune_rf, paste0(rds_path, "tune_rf"))

} else {

    tune_rf = readRDS(paste0(rds_path, "tune_rf"))

}

#assign hyperparameters to learner ----------------------------------------------

classif.lrn.optimised = setHyperPars(classif.lrn, mtry = tune_rf$x$mtry, ntree = tune_rf$x$ntree)

# Tuning results -----------------------------------------------------------------
tune_rf$x # best found settings
tune_rf$y # estimated performance
data = generateHyperParsEffectData(tune_rf)
data$data
plt = plotHyperParsEffect(data, x = "mtry", y = "acc.test.mean")
plt + ggtitle("Random Forest mtry") +
    theme_bw()

plt = plotHyperParsEffect(data, x = "ntree", y = "acc.test.mean")
plt + ggtitle("Random Forest ntree") +
    theme_bw()

plt = plotHyperParsEffect(data, x = "ntree", y = "mmce.test.mean")
plt + ggtitle("ntree misclassification") +
    theme_bw()



