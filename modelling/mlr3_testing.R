# Testing mlr3

library(mlr3)
library(mlr3viz)
library(data.table)
library(mlr3learners)
library(precrec)
library(mlr3spatiotempcv)
library(mlr3filters)

# shipped tasks for trial
mlr_tasks
as.data.table(mlr_tasks)

# get mtcars
data("mtcars", package = "datasets")
data = mtcars[, 1:3]
str(data)

task_mtcars = TaskRegr$new(id = "cars", backend = data, target = "mpg")
print(task_mtcars)

# plot for overview
autoplot(task_mtcars, type = "pairs")


# get iris
task_iris = mlr_tasks$get("iris")
print(task_iris)
task_iris$data()

print(task_iris$col_roles)
autoplot(task_iris, type = "duo")
# ------------------------------------------------------------------------------

# classif random forest
mlr3learners::LearnerClassifRanger
learner = mlr_learners$get("classif.ranger")
print(learner)

learner$param_set$upper

lrn("classif.ranger")

# ------------------------------------------------------------------------------

# task definition:
task = tsk("sonar")

## ways to specify a learner:
learner = lrn("classif.rpart")
learner = lrn("classif.rpart", predict_type = "prob")
learner$predict_type = "prob"


# simple, yet better resampling in section 2.5 in the mlr3 book
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)
# better done with respampling


# training:
learner$model #before
# here training the learner
learner$train(task, row_ids = train_set)
learner$model #after


# prediction
prediction = learner$predict(task, row_ids = test_set)
prediction
prediction$confusion
prediction$response
prediction$prob

# access now ->
end = as.data.table(prediction)
end

# ------------------------------------------------------------------------------
# plotting classif task
autoplot(prediction)
autoplot(prediction, type = "roc")

# ------------------------------------------------------------------------------
# plotting a regression
local({ # we do this locally to not overwrite the objects from previous chunks
    task = tsk("mtcars")
    learner = lrn("regr.lm")
    learner$train(task)
    prediction = learner$predict(task)
    autoplot(prediction)
})
# -----------------------------------------------------------------------------
# Performance measurement

mlr_measures
measure = msr("classif.acc")
measure2= msr("classif.ce")
prediction$score(measure)
prediction$score(measure2)


#####################################################
# Resampling

# overview
as.data.table(mlr_resamplings) # error here! -> Patrick
mlr_resamplings

resampling = rsmp("holdout", ratio = 0.8)
# or:
resampling$param_set$values = list(ratio =  0.7)
resampling = rsmp("repeated-spcv-coords", folds = 10, repeats = 10)
resampling = rsmp("cv", folds = 3L)

# now give the order! INSTANTIATE!
resampling$instantiate(task_iris)
resampling$iters

# inspect the train and test set:
resampling$test_set(1)
resampling$train_set(1)

# execution -> ResampleResult object
task = tsk("pima")
learner = lrn("classif.rpart", maxdepth = 3, predict_type = "prob")
resampling = rsmp("cv", folds = 3L)

# resampling is the same as train and predict, but multiple times!
rr = resample(task, learner, resampling, store_models = TRUE)
print(rr)

# performance across the resammpling iterations
rr$aggregate(msr("classif.ce"))
# for indiv iter:
rr$score(msr("classif.ce"))

lrn = rr$learners[[1]]
lrn$model
rr$prediction() # all predictions merged into a single Prediction
rr$predictions()[[1]] # prediction of first resampling iteration

#### this is the prediction result!
prediction = rr$prediction()

# plotting resampling results:
autoplot(rr)
?autoplot.ResampleResult # -> available plots

# ------------------------------------------------------------------------------
# feature selection

filter = FilterJMIM$new()

task = tsk("iris")
filter$calculate(task)

as.data.table(filter)
mlr3filters::FilterImportance

# MY LEARNER -------------------------------------------------------------------

learner = lrn("classif.ranger", importance = "impurity")
# Now you can use the mlr3filters::FilterImportance class for algorithm-embedded methods to filter a Task.
task = tsk("iris")
filter = flt("importance", learner = lrn)
filter$calculate(task)
head(as.data.table(filter), 3)
