#library(mlr)
library(data.table)
library(parallelMap)
library(raster)
library(ranger)
library(rgdal)

# data input
data_input <- brick("/pvdata3/marcel/04_RF_woody_cover_KNP_v6/03_prediction_data/S1_A_VH_VV_mosaic_KNP_Area_no_nan_tile_6_1_tile_0_0")
mymodel <- readRDS("/pvdata3/marcel/04_RF_woody_cover_KNP_v6/01_lidar_class_S1_15_17/03_output_training_ranger/rf_train_ranger.rda")

# change projection to UTM
#data_input <- projectRaster(data_input, crs="+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

data_input[data_input == -99] <- NA
#data_input[data_input == 0] <- NA

# create data frame
data <- as.data.frame(data_input, xy=TRUE)

# change data frame names
setnames(data, c("x", "y", "VH_20150507", "VH_20150519", "VH_20150531", "VH_20150612","VH_20150706","VH_20150718","VH_20150730","VH_20150823","VH_20150916","VH_20150928","VH_20151103","VH_20151115","VH_20151127","VH_20151209","VH_20151221","VH_20160102","VH_20160114","VH_20160126","VH_20160207","VH_20160302","VH_20160314","VH_20160326","VH_20160407","VH_20160419","VH_20160501","VH_20160513","VH_20160525","VH_20160606","VH_20160630","VH_20160712","VH_20160724","VH_20160805","VH_20160817","VH_20160829","VH_20160910","VH_20160922","VH_20161004","VH_20161016","VH_20161028","VH_20161109","VH_20161121","VH_20161203","VH_20161215","VH_20161227","VH_20170108","VH_20170120","VH_20170201","VH_20170213","VH_20170225","VH_20170309","VH_20170321","VH_20170402","VH_20170414","VH_20170426","VV_20150507","VV_20150519","VV_20150531","VV_20150612","VV_20150706","VV_20150718","VV_20150730","VV_20150823","VV_20150916","VV_20150928","VV_20151103","VV_20151115","VV_20151127","VV_20151209","VV_20151221","VV_20160102","VV_20160114","VV_20160126","VV_20160207","VV_20160302","VV_20160314","VV_20160326","VV_20160407","VV_20160419","VV_20160501","VV_20160513","VV_20160525","VV_20160606","VV_20160630","VV_20160712","VV_20160724","VV_20160805","VV_20160817","VV_20160910","VV_20160922","VV_20161004","VV_20161016","VV_20161028","VV_20161109","VV_20161121","VV_20161203","VV_20161215","VV_20161227","VV_20170108","VV_20170120","VV_20170201","VV_20170213","VV_20170225","VV_20170309","VV_20170321","VV_20170402","VV_20170414","VV_20170426"))

# delete NA
data1 = na.omit(data)

saveRDS(data1, "/pvdata3/marcel/04_RF_woody_cover_KNP_v6/03_prediction_data/S1_A_VH_VV_mosaic_KNP_Area_no_nan_tile_6_1_tile_0_0.rda")

#data1 = readRDS("/pvdata3/marcel/04_RF_woody_cover_KNP_v6/03_prediction_data/S1_A_VH_VV_mosaic_KNP_Area_no_nan_tile_6_1_tile_0_0.rda")

##### PREDICT

#result <- predict(data_input, mymodel, filename="E:/04_RF_woody_cover_KNP/03_output_ranger_predict/S1_A_VH_mtsf_stack_tile_05_06_08_09_no17_mult_temp_stats_subset_ranger_rf", progress='text', type='response', format="GTiff", datatype='INT2U', overwrite=TRUE, na.rm=TRUE)
result <- predict(mymodel, data1, verbose=TRUE, num.threads = 20)

##### PREDICT END

##### OUTPUT
#result_xy =  cbind(data1, result$predictions)
result_xy = cbind(data1$x, data1$y, result$predictions)
result_xy = as.data.frame(result_xy)
setnames(result_xy, c("x", "y","class"))
coordinates(result_xy) <- ~x+y
gridded(result_xy) <- TRUE
projection(result_xy) <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#projection(result_xy) <- CRS("+proj=longlat +datum=WGS84")

class(result_xy)

outfile <- raster(result_xy) 
writeRaster(outfile, filename = "/pvdata3/marcel/04_RF_woody_cover_KNP_v6/01_lidar_class_S1_15_17/04_prediction_output/S1_A_VH_VV_mosaic_KNP_Area_no_nan_tile_6_1_tile_0_0.tif", format="GTiff", datatype='INT2U', overwrite=TRUE, na.rm=TRUE)



#writeGDAL(outfile, fname = "/pvdata3/marcel/01_RF_woody_cover_KNP/test_output.tif", drivername="GTiff", type='Float32')

##### OUTPUT END






