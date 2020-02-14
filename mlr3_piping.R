#' In order to run the prediction multiple subsets of the study area had to be made. Otherwise the memory
#' would overrun and cannot allocate space.
#' Konstantin Schellenberg, 14.02.2020

source("import.R")
library(RColorBrewer)

# file_descriptor = "lefttop"
#
#
# # input tables
# input = as.data.table(readRDS(paste0(path_rds, "input.rds"))) %>%
#     dplyr::select(contains(c("vh", "red", "nir")), "x", "y", "class") %>%
#     dplyr::select(sort(names(.)))
#
# newdata.split1 = as.data.table(readRDS(paste0(path_rds, "splits/", file_descriptor, "_split1.rds"))) %>%
#     dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
#     dplyr::select(sort(names(.)))
#
# newdata.split2 = as.data.table(readRDS(paste0(path_rds, "splits/", file_descriptor, "_split2.rds"))) %>%
#     dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
#     dplyr::select(sort(names(.)))


classif = function(input, newdata, outfile){

    # make task
    task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "class",
                                      coordinate_names = c("x", "y"),
                                      crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")

    # define learner
    learner = lrn("classif.ranger", predict_type = "prob")

    # set built-in filter & hyperparameters
    learner$param_set$values = list(num.trees =500L, mtry = 4)

    # multicore application
    future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

    # train
    learner$train(task_slangbos)

    # prediction
    pred = learner$predict_newdata(task = task_slangbos, newdata = newdata)

    output = data.table::as.data.table(pred)
    exporting(output = output, input = newdata, filepath = paste0(path_prediction, outfile))

}

# Iterate of the splite------------------------------------------------------------------------------

input_splits = list.files(paste0(path_rds, "splits"))

for (i in input_splits){


    # concat strings
    name = substr(i, start = 1, stop = nchar(i) - 4)
    output = paste0("predict_vrn_", name)

    # load data
    split = as.data.table(readRDS(paste0(path_rds, "splits/", i))) %>%
        dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
        dplyr::select(sort(names(.)))

    # print information
    cat("predicting on ", name)

    # prediction with helper function classif {global-env} -> functions.R
    classif(input = input, newdata = split, outfile =  paste0("predict_vrn_", substr(i, start = 1, stop = nchar(i) - 4)))

}

classif(input = input, newdata = newdata.split2, outfile =  "predict_vrn_lefttop_split2")


# measure of resampling
# load measures
rr_vh = readRDS(paste0(path_developement, "rf_acc/ResamplingResult_vrn.rds"))

# accuracies
rr_vh$aggregate(measures = msr("classif.acc"))
rr_vh$aggregate()

autoplot(resampling, task_slangbos)
autoplot(rr_vh, type = "histogram", bins = 20L)

# predict
pred = rr_vh$prediction()

autoplot(pred)

# make confusion scaled --------------------------------------------------------

confusion = pred$confusion

normalise = scale(confusion, center=FALSE, scale=colSums(a)) * 100

# plot
ggplot(data = as.data.frame(normalise), mapping = aes(x = truth, y = response)) +
    geom_tile(aes(fill = Freq), colour = "grey") +
    geom_text(aes(label = sprintf("%.1f", Freq)), vjust = 1)+
    scale_fill_gradientn(colours = brewer.pal(9, name = "YlOrRd"))
