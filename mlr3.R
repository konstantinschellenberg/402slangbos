#' mlr3 version of slangbos classifiction
#' benefit: can handle data.tables and is thus meant to fit in the memory
#' https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html


source("import.R")

# DATA -------------------------------------------------------------------------

# ground truthing
plot(gt[1])

file_descriptor = "lefttop_split1"

# input tables
input = as.data.table(readRDS(paste0(path_rds, "input.rds"))) %>%
    dplyr::select(contains(c("vh", "red", "nir")), "x", "y", "class") %>%
    dplyr::select(sort(names(.)))

newdata.split1 = as.data.table(readRDS(paste0(path_rds, "splits/", file_descriptor, ".rds"))) %>%
    dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
    dplyr::select(sort(names(.)))

# newdata.split2 = as.data.table(readRDS(paste0(path_rds, "splits/", file_descriptor, ".rds"))) %>%
#     dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
#     dplyr::select(sort(names(.)))

ncol(newdata.split1)
ncol(input)

# CREATE TASK ------------------------------------------------------------------
mlr_reflections$task_types # task types available
task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "class",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")

# inspect task -----------------------------------------------------------------

task_slangbos$backend$colnames
task_slangbos$backend

task_slangbos$class_names
task_slangbos$properties

task_slangbos$task_type
task_slangbos$coordinate_names

mlr_reflections$task_col_roles$classif # var roles for classification
learner$param_set$ids()

autoplot(task_slangbos) # distribution of samples
# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("classif.ranger", predict_type = "prob")

# set built-in filter & hyperparameters
learner$param_set$values = list(num.trees =500L, mtry = 4, importance = "impurity")



# optional: FILTERING ----------------------------------------------------------

local({

    filter = flt("importance", learner = learner)
    filter$calculate(task_slangbos)


    head(as.data.table(filter), 50)
    tail(as.data.table(filter), 50)

    a = as.data.table(filter)
    b = substr(a$feature, start = 1,stop = 3) %>%
        as.factor()

    plot(x = a$score, pch = 16, col = as.factor(b))
})


# TUNING seperately ------------------------------------------------------------

tune = ParamSet$new(list(
    ParamInt$new("num.trees", lower = 1, upper = 500),
    ParamInt$new("mtry", lower = 1, upper = 4)
))

measures = msr("classif.ce")
terminator = term("evals", n_evals = 20)
tuner = tnr("grid_search", resolution = 5)
resampling = rsmp("repeated-spcv-coords", folds = 6L, repeats = 10L)


instance = TuningInstance$new(
    task = task_slangbos,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = tune,
    terminator = term
)

result = tuner$tune(instance)

# results:
instance$archive(unnest = "params")[, c("num.trees", "mtry", "classif.ce")]

# TUNING automatically ---------------------------------------------------------

at = AutoTuner$new(
    learner = learner,
    resampling = resampling,
    measures = measures,
    tune_ps = tune,
    terminator = terminator,
    tuner = tuner
)
at$train(task_slangbos)

saveRDS(at, paste0(path_rds, "automatedTunedLearner_eval20.rds"))

at = readRDS(paste0(path_rds, "automatedTunedLearner_eval20.rds"))

at$predict(task_slangbos)

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

as.data.table(mlr_resamplings) # error -> report Patrick S
mlr_resamplings

# setup resampling task
resampling = rsmp("repeated-spcv-coords", folds = 6L, repeats = 10L)
resampling$instantiate(task_slangbos)
resampling$iters
resampling$

# splitting task in train and test
str(resampling$train_set(1))
str(resampling$test_set(1))

# run: TIME INTENSIVE
rr = mlr3::resample(task_slangbos, learner, resampling, store_models = TRUE)

# save result:
c = 0
if (c == 1) {
    write_rds(rr, paste0(path_developement, "rf_acc/ResamplingResult_vrn.rds"))
    rr_vh = readRDS(paste0(path_developement, "rf_acc/ResamplingResult_vrn.rds"))
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

# save model:
saveRDS(learner, paste0(path_rds, "Learner.rds"))

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
# ------------------------------------------------------------------------------


pred = learner$predict_newdata(task = task_slangbos, newdata = newdata)

pred$confusion
pred$prob
pred$response


### Stats on the prediction
autoplot(pred)
head(as.data.table(pred))

# EXPORT -----------------------------------------------------------------------

output = data.table::as.data.table(pred)

exporting(output = output, input = newdata.split2, filepath = paste0(path_prediction, "02-12_rightbottom_split1_red_nir_vh"))
