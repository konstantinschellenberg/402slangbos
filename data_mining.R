# watch the video for introduction in RF for remote sensing
# https://www.youtube.com/watch?v=fal4Jj81uMA

# for tuning
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# ------------------------------------------------------------------------------
library(sf)
library(rgdal)
library(raster)
library(caret)

# ------------------------------------------------------------------------------
training_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"
s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"

# import raster
s1vv = brick(s1vv_path)
s1vh = brick(s1vh_path)
names(s1vv)

# Renaming------------------------------------------------------------------------------

rename_bandnames = function(raster = s1vv){

    bandnames = names(raster)

    # iterate for date in column-names
    for (i in bandnames){
        date_in_bandnames = substr(bandnames,13,20)
    }

    # convert date string into R date-time format
    date <- c()
    for (i in 1:length(date_in_bandnames)){
        date <- append(date, as.POSIXct(date_in_bandnames[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }

    names(raster) <- paste0(date) # change names to more easy
    return(raster)
}

# remove data with little coverage of the region of interest
raster = rename_bandnames(raster = s1vh) %>%
    .[[c(-14, -17, -62)]]

#subsetting raster for easier calculation. Max. variables for rf is 32
raster = raster[[1:30]]

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
modFit_rf <- train(as.factor(class) ~ ., method = "rf", data = sdfAll)

# predict model
beginCluster()
system.time(preds_rf <- clusterR(raster, raster::predict, args = list(model = modFit_rf)))
endCluster()

# write as file
raster::writeRaster(preds_rf, "D:\\Geodaten\\#Jupiter\\GEO402\\04_products\\rf\\preds_rf_20200111_vh_sample30_rm.NA.tif", overwrite=TRUE)

