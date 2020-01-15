# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to run the random forest classification.


#____________________________________________________________
# watch the video for introduction in RF for remote sensing
# https://www.youtube.com/watch?v=fal4Jj81uMA

# for tuning
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

source("import.R")
library(sf)
library(rgdal)
library(raster)
library(caret)

# Renaming------------------------------------------------------------------------------

# remove data with little coverage of the region of interest
raster = rename_bandnames(raster = s1vh) %>%
    .[[c(-14, -17, -62)]]

#subsetting raster for easier calculation. Max. variables for rf is 32
raster = raster[[1:10]]

# beginCluster()
# raster[raster == -99] = NA
# endCluster()

# Read in training data-----------------------------------------------------------

# set crs(roi) to the crs(s1) brick. Remove Z-Dimension
trainData_sf =  st_read(training_path) %>%
    st_transform(st_crs(raster)) %>%
    st_zm(drop = TRUE)

trainData = as(trainData_sf, Class = "Spatial")
responseCol = "Name"

# Extracting training pixel values
# creating new df to fit pixel extraction
# credits to http://amsantac.co/blog/en/2015/11/28/classification-r.html
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(raster)) + 1))

for (i in 1:length(unique(trainData[[responseCol]]))){
    category <- unique(trainData[[responseCol]])[i]
    categorymap <- trainData[trainData[[responseCol]] == category,]
    dataSet <- extract(raster, categorymap)
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
}

# subsetting

nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]

# train model
modFit_rf <- train(as.factor(class) ~ ., method = "rf", data = sdfAll,
                   importance=TRUE,
                   trControl=trainControl(method="cv",number=5))

# predict model
beginCluster()
system.time(preds_rf <- clusterR(raster, raster::predict, args = list(model = modFit_rf)))
endCluster()

# write as file
raster::writeRaster(preds_rf, "D:\\Geodaten\\#Jupiter\\GEO402\\04_products\\rf\\preds_rf_20200112_vh_sample10_CV.tif", overwrite=TRUE)

# spacetime
# blast
# CAST
# stars

