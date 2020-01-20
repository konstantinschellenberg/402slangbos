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
