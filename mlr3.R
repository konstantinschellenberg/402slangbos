#' mlr3 version of slangbos classifiction
#' benefit: can handle data.tables and is thus meant to fit in the memory
#' https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html


source("import.R")

# DATA -------------------------------------------------------------------------

# ground truthing
plot(gt[1])

# input tables
input = as.data.table(readRDS(paste0(path_rds, "input.rds"))) %>%
    dplyr::select(contains("red"), "x", "y", "class") %>%
    dplyr::select(sort(names(.)))

newdata.split1 = as.data.table(readRDS(paste0(path_rds, "newdata_split1_datatable.rds"))) %>%
    dplyr::select(contains("red"), "x", "y") %>%
    dplyr::select(sort(names(.)))

newdata.split2 = readRDS(paste0(path_rds, "newdata_split2_datatable.rds")) %>%
    dplyr::select(contains("vh"), "x", "y") %>%
    dplyr::select(sort(names(.)))

ncol(newdata.split1)
ncol(input)

task_slangbos$backend


# Overview ---------------------------------------------------------------------

# Possible tasks
mlr_reflections$task_types # task types available
# inspect
task_slangbos$backend$colnames

task_slangbos$class_names
task_slangbos$properties

task_slangbos$task_type
task_slangbos$coordinate_names

mlr_reflections$task_col_roles$classif # var roles for classification
learner$param_set$ids()

autoplot(task_slangbos) # distribution of samples

# CREATE TASK ------------------------------------------------------------------

task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "class",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")


# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("classif.ranger", predict_type = "prob")

# set built-in filter & hyperparameters
learner$param_set$values = list(num.trees =500L, mtry = 4)

# optional: FILTERING ----------------------------------------------------------

# local({
#     filter = flt("importance", learner = learner)
#     filter$calculate(task_slangbos)
#     a = as.data.table(filter)
#
#     head(as.data.table(filter), 50)
#     tail(as.data.table(filter), 50)
#
#     b = substr(a$feature, start = 1,stop = 3) %>%
#         as.factor()
#
#
#     plot(x = a$score, pch = 16, col = as.factor(b))
# })

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

as.data.table(mlr_resamplings) # error -> report Patrick S
mlr_resamplings

# setup resampling task
resampling = rsmp("repeated-spcv-coords", folds = 10L, repeats = 10L)
resampling$instantiate(task_slangbos)
resampling$iters

# splitting task in train and test
str(resampling$train_set(1))
str(resampling$test_set(1))

# run: TIME INTENSIVE
rr = mlr3::resample(task_slangbos, learner, resampling, store_models = TRUE)

# save result:
c = 0
if (c == 1) {
    write_rds(rr, paste0(path_developement, "rda/ResamplingResult.rds"))
    rr_vh = readRDS("D:/Geodaten/#Jupiter/GEO402/03_develop/rf_acc/ResamplingResultVH.rds")
}

### results
mlr_measures
rr$aggregate(measures = msr("classif.acc"))
rr$aggregate()
rr$score(msr("classif.acc"))

### plotting resampling results
# spatial cross-validation Brenning et al.
autoplot(resampling, task_slangbos)

# metrics
autoplot(rr)
autoplot(rr, type = "histogram", bins = 30L)

# PREDICT ON TEST --------------------------------------------------------------
# learner$predict_type = "prob"

pred_test = rr$prediction()

head(fortify(pred_test))

as.data.table(pred_test)

pred_test$confusion
pred_test$prob
pred_test$response

### results
pred_test$score(msr("classif.acc"))
autoplot(pred_test)

# TRAIN ------------------------------------------------------------------------

learner$train(task_slangbos)
print(learner$model)

# PREDICT ----------------------------------------------------------------------

#' * `predict_newdata(newdata, task = NULL)`\cr
#'   (`data.frame()`, [Task]) -> [Prediction]\cr
#'   Uses the model fitted during `$train()` in to create a new [Prediction] based on the new data in `newdata`.
#'   Object `task` is the task used during `$train()` and required for conversions of `newdata`.
#'   If the learner's `$train()` method has been called, there is a (size reduced) version of the training task stored in the learner.
#'   If the learner has been fitted via [resample()] or [benchmark()], you need to pass the corresponding task stored
#'   in the [ResampleResult] or [BenchmarkResult], respectively.

# Piped version, easier:
# pred = learner$train(task_slangbos)$predict_newdata(newdata = newdata.split1)

pred = learner$predict_newdata(newdata.split1)

learner$help()

pred$confusion
pred$prob
pred$response


### Stats on the prediction
autoplot(pred)
head(as.data.table(pred))

# EXPORT -----------------------------------------------------------------------

output = data.table::as.data.table(pred)

newdata$x
newdata$y


exporting(output = output, input = newdata.split1, filepath = paste0(path_prediction, "02-12_red"))
