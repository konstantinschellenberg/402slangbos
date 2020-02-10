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

# DATA -------------------------------------------------------------------------

plot(gt[1])

data = dt



# CREATE TASK ------------------------------------------------------------------
# data = as.data.table.raster(vh, xy = TRUE, inmem = F)

?TaskClassifST

task_slangbos = TaskClassifST$new(id = "slangbos", backend = data, target = "class",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")



# Opportunities
mlr_reflections$task_types # task types available
mlr_reflections$task_col_roles$classif # var roles for classification


# inspect
tail(task_slangbos$backend$colnames)
task_slangbos$class_names
task_slangbos$properties
task_slangbos$ncol
task_slangbos$backend$colnames
task_slangbos$task_type
task_slangbos$class_names
task_slangbos$positive
task_slangbos$negative
task_slangbos$coordinate_names

autoplot(task_slangbos)

# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("classif.ranger", predict_type = "response")

# set hyperparameters
learner$param_set$values = list(num.trees =500L, mtry = 4)
# Now you can use the mlr3filters::FilterImportance class for algorithm-embedded methods to filter a Task.

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess")

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
autoplot(rr)
autoplot(pred)


# TRAIN ------------------------------------------------------------------------
learner$train(task_slangbos)


# PREDICT ----------------------------------------------------------------------
# learner$predict_type = "prob"

p = learner$predict(task_slangbos)
autoplot(p, type = pair)

?autoplot.PredictionClassif()



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

# EXPORT -----------------------------------------------------------------------

result_xy = cbind(pred, raster_input$x, raster_input$y) %>%
    as.data.frame() %>%
    dplyr::select(x = "raster_input$x", y = "raster_input$y", class = response)

# sf for coordnate system
out_sf = st_as_sf(result_xy, coords = c("x", "y"))
st_crs(out_sf) = 32735

# to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
out_sp = as(out_sf, "Spatial")
gridded(out_sp) = TRUE
class(out_sp)

outfile = raster(out_sp) %>%
    trim()

writeRaster(outfile, filename = paste0(prediction_out_path, "0121_prediction_rf_crop"),
            format="GTiff", datatype='INT1U', overwrite=TRUE, na.rm=TRUE)
