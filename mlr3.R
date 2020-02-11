#' mlr3 version of slangbos classifiction
#' benfit: can handle data.tables and is thus meant to fit in the memory
#' https://mlr3spatiotempcv.mlr-org.com/articles/mlr3spatiotempcv.html


source("import.R")

# DATA -------------------------------------------------------------------------

# ground truthing
plot(gt[1])

# input table
input

# Overview ---------------------------------------------------------------------

# Possible tasks
mlr_reflections$task_types # task types available



# CREATE TASK ------------------------------------------------------------------

task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "class",
                                  coordinate_names = c("x", "y"),
                                  crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")
mlr_reflections$task_col_roles$classif # var roles for classification

# inspect
task_slangbos$backend$colnames
task_slangbos$ncol
task_slangbos$class_names
task_slangbos$properties

task_slangbos$task_type
task_slangbos$class_names
task_slangbos$coordinate_names

autoplot(task_slangbos) # distribution of samples


# CREATE LEARNER ---------------------------------------------------------------

learner = lrn("classif.ranger", predict_type = "prob")

# set built-in filter & hyperparameters
learner$param_set$values = list(num.trees =500L, mtry = 4)
learner

# optional: FILTERING
local({
   filter = flt("importance", learner = learner)
filter$calculate(task_slangbos)
a = as.data.table(filter)

head(as.data.table(filter), 50)
tail(as.data.table(filter), 50)

b = substr(a$feature, start = 1,stop = 3) %>%
    as.factor()


plot(x = a$score, pch = 16, col = as.factor(b))
})

# RESAMPLE ---------------------------------------------------------------------

future::plan("multiprocess")

as.data.table(mlr_resamplings) # error
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

autoplot(pred_test)

# TRAIN ------------------------------------------------------------------------

learner$train(task_slangbos)
print(learner$model)

# PREDICT ----------------------------------------------------------------------

# Piped version, easier:
# pred = learner$train(task_slangbos)$predict(task_slangbos)

# get data
newdata = newdata


pred = learner$predict_newdata(task = task_slangbos,
                               newdata = newdata)
pred$confusion
pred_test$prob
pred_test$response


### Stats on the prediction
autoplot(pred)
head(as.data.table(pred))

# EXPORT -----------------------------------------------------------------------

output = data.table::as.data.table(pred)

newdata$x
newdata$y


exporting(output = output, input = newdata, filepath = paste0(path_prediction, "02-11_training"))

#' input needs to have coordinates stored as "x" and "y"
#' output is data.table

# not yet functioning...

exporting = function(output, input, filepath){

    # bind coords on data.table
    out5 = cbind(output, x = input$x, y = input$y)

    # make sf coords
    out4 = st_as_sf(out5, coords = c("x", "y"))

    # set crs
    st_crs(out4) = 32735

    # to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
    out3 = as(out4, "Spatial")

    # gridding
    gridded(out3) = TRUE
    class(out3)

    outfile = stack(out3) %>%
        trim()

    writeRaster(outfile, filename = paste0(prediction_out_path, filepath, "tif"),
                format="GTiff", datatype='FLT4S', overwrite=TRUE, na.rm=TRUE)
}
