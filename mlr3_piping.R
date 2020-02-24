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
inner = readRDS(paste0(path_developement, "rf_acc/ResamplingResult_vrn.rds"))

# accuracies
inner$aggregate(measures = msr("classif.acc"))
inner$aggregate()

autoplot(resampling, task_slangbos)
autoplot(inner, type = "histogram", bins = 20L)

# predict
pred = inner$prediction()

autoplot(pred)

# CONFUSION MATRIX -------------------------------------------------------------
# read in
confus = readRDS(paste0(path_developement, "rf_acc/confusion_outer.rda"))

# coloums and rows:
nm = c("Increase", "Continuous", "Breakpoint", "Agriculture", "Bare Soil",
       "Grassveld", "Forest", "Urban", "Water")

# inner.confus = pred$confusion
# inner.confus.norm = scale(inner.confus, center=FALSE, scale=colSums(inner.confus)) * 100

rownames(confus) = nm
colnames(confus) = nm
inp = as.data.frame(confus)

# plot
ggplot(data = as.data.frame(inp), mapping = aes(x = inp[,1], y = rev(inp[,2])))+
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%.0f", Freq)), vjust = 0.5, colour = "black")+
    scale_fill_gradientn(colours = rev(viridis::inferno(8, begin = 0.45)))+
    xlab("Truth/Reference")+
    ylab("Response")+
    labs(fill = "Response [%]")+
    scale_y_discrete(labels = rev(nm))+
    theme(axis.text.x = element_text(family = "serif", face="bold", color="grey20",
                                     size=8, angle=90),
          axis.text.y = element_text(family = "serif", face="bold", color="grey20",
                                     size=8, angle=0),
          axis.title.x = element_text(family = "serif", face="bold", color="black",
                                      size=10, angle=0),
          axis.title.y = element_text(family = "serif", face="bold", color="black",
                                      size=10, angle=90),
          aspect.ratio = 1,
          legend.position = "")
)




# plot confusion matrix after scaling ------------------------------------------
# OUTER

outer.confus = read.csv(file = "D:/Geodaten/#Jupiter/GEO402/04_products/performance/outer_confusion.csv", sep = " ", header = T)
outer.confus = outer.confus[1:9,1:9]
colnames(outer.confus) = c(1:9)

outer.confus.norm = scale(outer.confus, center=FALSE, scale=colSums(outer.confus)) * 100
outer.confus.norm = as.table(outer.confus.norm)

rownames(outer.confus.norm) = nm
colnames(outer.confus.norm) = nm
outer.confus.norm


# plot
ggplot(data = as.data.frame(outer.confus.norm), mapping = aes(x = Var2, y = Var1)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%.0f", Freq)), vjust = 0.5, colour = "black")+
    scale_fill_gradientn(colours = colfunc(8))+
    xlab("Truth/Reference")+
    ylab("Response")+
    labs(fill = "Response on truth [%]")+
    theme(axis.text.x = element_text(face="bold", color="black",
                                             size=9, angle=30),
                  axis.text.y = element_text(face="bold", color="black",
                                             size=9, angle=30))
