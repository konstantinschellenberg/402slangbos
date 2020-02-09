#' MLR testing spatial modelling
#' https://mlr.mlr-org.com/articles/tutorial/handling_of_spatial_data.html
#'

source("import.R")

library(mlr)
library(cowplot)


data("spatial.task")
spatial.task

learner.rf = makeLearner("classif.ranger", predict.type = "prob")

resampling = makeResampleDesc("SpRepCV", fold = 5, reps = 5)

set.seed(123)
out = resample(learner = learner.rf, task = spatial.task,
               resampling = resampling, measures = list(auc))

rdesc1 = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
r1 = resample(makeLearner("classif.qda"), spatial.task, rdesc1, show.info = FALSE)
rdesc2 = makeResampleDesc("RepCV", folds = 5, reps = 4)
r2 = resample(makeLearner("classif.qda"), spatial.task, rdesc2, show.info = FALSE)

plots = createSpatialResamplingPlots(spatial.task,
                                     list("SpRepCV" = r1, "RepCV" = r2), crs = 32717, repetitions = 1,
                                     x.axis.breaks = c(-79.075), y.axis.breaks = c(-3.975))
plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 2,
          labels = plots[["Labels"]], label_size = 8)
