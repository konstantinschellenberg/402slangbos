######################################################################
# Subtle features in slangbos encroachment, SA
#
# based on script by Urban, M. and Schratz, P.
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de),
# Patrick Schratz (p.schratz@lmu.de) and Konstantin Schellenberg (konstantin.schellenberg@posteo.de)
#
# Step 3/4
# Training & Prediction

source("02_cross_val.R")

# remove test file
# file.remove(rds_path, "learning_input_test.rds")

classif.task # Task
classif.lrn.optimised # Learner
classif.lrn.optimised$par.set # Parameters

# training and validation split---------------------------------------------------

n = getTaskSize(classif.task)
training.set = sample(n, size = n/3)
validation.set = sample(n, size = n*0.6) # not working

# Training----------------------------------------------------------------------

parallelStart(mode = "socket", level = "mlr.resample", cpus = 8)
model = train(classif.lrn.optimised, classif.task, subset = training.set)
parallelStop()

# saveRDS(model, paste0(rds_path, "model_s2"))
model = readRDS(paste0(rds_path, "model_s2"))

model$factor.levelsmodel
model$task.desc
model$features
model$subset # no subset
model$learner #type of model

# wrapped model for predictions

getLearnerModel(model)

