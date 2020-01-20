######################################################################
# Subtle features in slangbos encroachment, SA
#
# based on script by Urban, M. and Schratz, P.
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de),
# Patrick Schratz (p.schratz@lmu.de) and Konstantin Schellenberg (konstantin.schellenberg@posteo.de)
#
# Step 3/4
# Training

source("02_cross_val.R")

# remove test file
# file.remove(rds_path, "learning_input_test.rds")

classif.lrn.optimised # Task
classif.lrn.optimised$par.set # Learner
classif.task

# ------------------------------------------------------------------------------

model = train(classif.lrn.optimised, classif.task)
model$factor.levels
model$task.desc
model$features
model$subset # no subset
model$learner.model

# ------------------------------------------------------------------------------

raster_test
raster = as.data.frame(raster_test, xy = TRUE)
head(as_tibble(raster))

pred.task = makeTask
prediction = predict(model, newdata = raster)

calculateConfusionMatrix(prediction)

plotLearnerPrediction(classif.lrn.optimised, task = classif.task)

# ------------------------------------------------------------------------------
##### OUTPUT

result_xy = cbind(raster$x, raster$y, prediction$predictions)
result_xy = as.data.frame(result_xy)
setnames(result_xy, c("x", "y","class"))
coordinates(result_xy) = ~x+y
gridded(result_xy) = TRUE
projection(result_xy) = CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#projection(result_xy) = CRS("+proj=longlat +datum=WGS84")

class(result_xy)

outfile = raster(result_xy)
writeRaster(outfile, filename = paste0(prediction_out_path, "prediction_rf", format="GTiff", datatype='INT2U', overwrite=TRUE, na.rm=TRUE)
