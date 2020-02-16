#' In order to run the prediction multiple subsets of the study area had to be made. Otherwise the memory
#' would overrun and cannot allocate space.
#' Konstantin Schellenberg, 14.02.2020

source("import.R")

# for tuned model:
# at = readRDS(paste0(path_rds, "automatedTunedLearner_eval20"))


# speed check for new tidytable package
# system.time(as_dt(readRDS(paste0(path_rds, "splits/", file_descriptor, "_split1.rds"))))
# system.time(as.data.table(readRDS(paste0(path_rds, "splits/", file_descriptor, "_split1.rds"))))


# Iterate to classify all splits------------------------------------------------------------------------------
input_splits = list.files(paste0(path_rds, "splits"))
for (i in input_splits){

    source("import.R")
    input_splits = list.files(paste0(path_rds, "splits"))

    input = as_dt(readRDS(paste0(path_rds, "input.rds"))) %>%
        dplyr::select(contains(c("vh", "red", "nir")), "x", "y", "class") %>%
        dplyr::select(sort(names(.)))

    # concat strings
    name = substr(i, start = 1, stop = nchar(i) - 4)
    output = paste0("predict_vrn_", name)

    # load data
    split = as_dt(readRDS(paste0(path_rds, "splits/", i))) %>%
        dplyr::select(contains(c("vh", "red", "nir")), "x", "y") %>%
        dplyr::select(sort(names(.)))

    # print information
    cat("predicting on", name, sep = "\n")

    # prediction with helper function classif {global-env} -> functions.R
    classif(newdata = split, path_model = paste0(path_rds, "Learner.rds"),
            outfile =  paste0("predict_vrn_test", substr(i, start = 1, stop = nchar(i) - 4)))

    # now clearing environment
    rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
    gc() #free up memrory and report the memory usage.

}


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
