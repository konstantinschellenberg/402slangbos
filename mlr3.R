#' mlr3 version of slangbos classifiction
#' benfit: can handle data.tables and is thus meant to fit in the memory
#' https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html

library(mlr3)
library(mlr3viz)
library(data.table)
library(mlr3learners)
library(precrec)
library(mlr3spatiotempcv)
library(mlr3filters)
library(ggplot2)


source("import.R")

# CREATE TASK ------------------------------------------------------------------
# data = as.data.table.raster(vh, xy = TRUE, inmem = F)

gt
data = dt
?TaskClassifST

mlr_reflections$task_types
mlr_reflections$task_col_roles
mlr_resamplings

task_slangbos = TaskClassifST$new(id = "slangbos", backend = data, target = "class",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")

# inspect
tail(task_slangbos$backend$colnames)
task_slangbos$class_names
task_slangbos$properties
task_slangbos$ncol

autoplot(task_slangbos)

# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("classif.ranger", predict_type = "response")

# set hyperparameters
learner$param_set$values = list(num.trees =500L, mtry = 4)
# Now you can use the mlr3filters::FilterImportance class for algorithm-embedded methods to filter a Task.

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess", strategy = )

mlr_resamplings
resampling = rsmp("repeated-spcv-coords", folds = 10, repeats = 5)

# resampling
rr = mlr3::resample(task_slangbos, learner, resampling, store_models = T)

rr$aggregate()
rr$aggregate(measures = msr("classif.acc"))

rr$score(msr("classif.acc"))

pred = rr$prediction()

pred$confusion

# plotting resampling results:
autoplot(rr, type = )

# TRAIN ------------------------------------------------------------------------
learner$train(task_slangbos)
p = learner$predict(task_slangbos)
autoplot(p, type = pair)

# PREDICT ----------------------------------------------------------------------




# FILTER -----------------------------------------------------------------------
filter = flt("importance", learner = lrn)
filter$calculate(task)
head(as.data.table(filter), 3)


learner = lrn("classif.ranger", importance = "impurity")
# Now you can use the mlr3filters::FilterImportance class for algorithm-embedded methods to filter a Task.
task = tsk("iris")
filter = flt("importance", learner = lrn)
filter$calculate(task)
head(as.data.table(filter), 3)


# test


set.seed(0)
values(r) <- runif(ncell(r))
hasValues(r)
## [1] TRUE
inMemory(r)
## [1] TRUE
values(r)[1:10]
plot(r)

r.dt = as.data.table.raster(red, xy = T)

write_rds(r.dt, "data/test.rds")
