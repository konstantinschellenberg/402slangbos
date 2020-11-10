#' Classification of Slangbos with mlr³
#' init: 10.11.2020, Konstantin Schellenberg

source("D:/Projects/402slangbos/import_rasters_classification.R")
source("D:/Projects/402slangbos/import_samples.R")

require(mlr3)
require(mlr3learners)
require(mlr3spatiotempcv)

# USER INPUT ################
input.in = read_rds("D:/Geodaten/GEO402/03_develop/classification/input_samples_2017-10-01.RDS")
newdata.in = read_rds("D:/Geodaten/GEO402/03_develop/classification/predictionset_2017-10-01.RDS")

prediction = "D:/Geodaten/GEO402/04_results/classification/pred_half2"

# subset newdata
delim = nrow(newdata.in) / 2
delim2 = nrow(newdata.in) / 4
delim3 = delim + delim2

newdata = newdata.in[delim3:nrow(newdata.in)]

input = input.in[ , classif:=as.factor(classif)]

# MODEL SETUP ###############

# make task
task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "classif",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")

# define learner
learner = lrn("classif.ranger", predict_type = "prob")

# set built-in filter & hyperparameters
cat("Creating Learner\n")
learner$param_set$values = list(num.trees =375L, mtry = 2L)

# multicore application
future::plan("multisession") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

# train
cat("Train Model\n")
learner$train(task_slangbos)

# prediction
# TODO: Swap partition vergrößern!
cat("Prediction . . .\n")
pred = learner$predict_newdata(task = task_slangbos, newdata = newdata)

cat("Converting to raster:\n")
output = data.table::as.data.table(pred)

cat("Exporting raster:\n")
exporting(output = output, input = newdata, filepath = prediction)

