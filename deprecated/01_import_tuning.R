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
library(stars)

source("import.R")
getOption("max.print")
options(max.print = 1000)


######################################################################
# Data Preparation
######################################################################

# create input tables

# full
gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")
gt_from_raster(raster = vv, train_data = gt, response_col = "Name", outfile = "vv")
# small
gt_from_raster(raster = vh_small, train_data = gt_smaller, response_col = "Name", outfile = "vh_sm")
gt_from_raster(raster = red_small, train_data = gt_smaller, response_col = "Name", outfile = "red_sm")
gt_from_raster(raster = nir_small, train_data = gt_smaller, response_col = "Name", outfile = "nir_sm")


# load input tables

# full
vh_input = readRDS(paste0(rds_path, "learning_input_vh.rds"))
red_input = readRDS(paste0(rds_path, "learning_input_red.rds"))
nir_input = readRDS(paste0(rds_path, "learning_input_nir.rds"))
vv_input = readRDS(paste0(rds_path, "learning_input_vv.rds"))
# small
vh_input = readRDS(paste0(rds_path, "learning_input_vh_sm.rds"))
red_input = readRDS(paste0(rds_path, "learning_input_red_sm.rds"))
nir_input = readRDS(paste0(rds_path, "learning_input_nir_sm.rds"))


# deleting previous:
x = 0
if (x == 1){
    file.remove(c(paste0(rds_path, "learning_input_vh.rds"),
                  paste0(rds_path, "learning_input_red.rds"),
                  paste0(rds_path, "learning_input_nir.rds")))
}



#################### FUNCTION HERE
# merge matrices, find out with cols are dublicates
dt3 = as_tibble(cbind(vh_input, vv_input), .name_repair = "unique")

# remove cols with x, y and class from the data frame, rename vars from the last binded data frame to x, y and class
# e.g. -vh_x, -vh_y, -class...356, -class...557, class = class...758, -red_x, -red_y, x = nir_x, y = nir_y
dt2 = dt3 %>%
    .[,1:(length(.) - 2)] %>%
    dplyr::select(-ends_with("x"), -ends_with("y"), -contains("class")) %>%
    cbind(dt3[,(length(dt3) - 2):length(dt3)]) %>%
    dplyr::rename(x = ends_with("x"),
                  y = ends_with("y"),
                  class = contains("class"))

# remove cols with NA (prerequisit for random forest input)
dt = dt2 %>%
    as.data.frame() %>%
    .[, colSums(is.na(.)) == 0] %>%
    as.data.table()

# number of variables:
length(names(dt))

# needs to be a factor:
if (!class(dt$class) == "factor"){
    warning("needs to be a factor!")
    }

class(dt)
names(dts)


# identical(dt$vh_x,dt$red_x,dt$nir_x) # check if coorinates are the same
#
# tibble::enframe(names(dt)) %>% count(value) %>% filter(n > 1) # check if columns are duplicates!

######################################################################
# Make Task
######################################################################

coords = as.data.frame(dt[c("x", "y")])
dt_coordsless = dplyr::select(dt, -x, -y) # removes coordinates from variable settings

classif.task = makeClassifTask(
    id = "slangbos", data = dt_coordsless, target = "class",
    coordinates = coords
)

predict.task = NA

classif.task = normalizeFeatures(classif.task, method = "standardize")
predict.task = normalizeFeatures(classif.task, method = "standardize")
######################################################################
# Filter
######################################################################

parallelStartSocket(cpus = 6, level = "mlr.selectFeatures")
fv = generateFilterValuesData(classif.task, method = "randomForest_importance")
parallelStop()

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
    parallelStart(mode = "socket", level = "mlr.tuneParams", cpus = 8, mc.set.seed = TRUE)

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
# tune_rf$x # best found settings
# tune_rf$y # estimated performance
# data = generateHyperParsEffectData(tune_rf)
# data$data
# plt = plotHyperParsEffect(data, x = "mtry", y = "acc.test.mean")
# plt + ggtitle("Random Forest mtry") +
#     theme_bw()
#
# plt = plotHyperParsEffect(data, x = "ntree", y = "acc.test.mean")
# plt + ggtitle("Random Forest ntree") +
#     theme_bw()
#
# plt = plotHyperParsEffect(data, x = "ntree", y = "mmce.test.mean")
# plt + ggtitle("ntree misclassification") +
#     theme_bw()



