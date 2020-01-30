######################################################################
# Subtle features in slangbos encroachment, SA
#
# based on script by Urban, M. and Schratz, P.
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de),
# Patrick Schratz (p.schratz@lmu.de) and Konstantin Schellenberg (konstantin.schellenberg@posteo.de)
#
# Step 3/3
# Training & Prediction

source("02_cross_val.R")

library(stars)

# remove test file
# file.remove(rds_path, "learning_input_test.rds")

classif.lrn.optimised # Task
classif.lrn.optimised$par.set # Learner
classif.task

#training and validation split---------------------------------------------------

n = getTaskSize(classif.task)
training.set = sample(n, size = n/3)
validation.set = sample(n, size = n*0.6) # not working

# Training----------------------------------------------------------------------

parallelStart(mode = "socket", level = "mlr.resample", cpus = 5)
model = train(classif.lrn.optimised, classif.task, subset = training.set)
parallelStop()

# saveRDS(model, paste0(rds_path, "model_VH"))
model = readRDS(paste0(rds_path, "model_VH"))

model$factor.levelsmodel
model$task.desc
model$features
model$subset # no subset
model$learner.model

# Prediction--------------------------------------------------------------------

raster_input = as.data.frame(s1vh_small, xy = TRUE)
# saveRDS(raster_input, file = paste0(rds_path, "rf_input.rds"))

# pred.task = makeTask
parallelStart(mode = "socket", cpus = 7)
prediction = predict(model, newdata = raster_input)
parallelStop()

pred = as.data.frame(prediction)

matrix = calculateConfusionMatrix(prediction)
matrix
plotLearnerPrediction(classif.lrn.optimised, task = classif.task)

ggplot(data = pred$data, aes(x = pred$data$truth,
                              y = pred$data$response))

# ------------------------------------------------------------------------------
##### OUTPUT

result_xy = cbind(pred, raster_input$x, raster_input$y) %>%
    as.data.frame() %>%
    dplyr::select(x = "raster_input$x", y = "raster_input$y", class = response)

# sf for coordnate system
out_sf = st_as_sf(result_xy, coords = c("x", "y"))
st_crs(out_sf) = 32735

out_sf$class = as.numeric(out_sf$class)
out_sf[out_sf$class == 2,1] = 12
out_sf[out_sf$class == 3,1] = 2
out_sf[out_sf$class == 4,1] = 3
out_sf[out_sf$class == 5,1] = 4
out_sf$class = as.factor(out_sf$class)

# to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
out_sp = as(out_sf, "Spatial")
gridded(out_sp) = TRUE
class(out_sp)

outfile = raster(out_sp) %>%
    trim()

writeRaster(outfile, filename = paste0(prediction_out_path, "0121_prediction_rf_crop"),
                                       format="GTiff", datatype='INT1U', overwrite=TRUE, na.rm=TRUE)
