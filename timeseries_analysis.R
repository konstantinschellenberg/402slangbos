#' Calculation of 5th and 95th Percentiles and median, intercepte and slope of multitemporal statistics
#' Konstantin Schellenberg, 06.05.2020
#' Chair of Remote Sensing, Institute for Geography, Friedrich-Schiller-University Jena
#' Supervisor: Dr. Marcel Urban

source("import.R")

file = vh
filename = "vh"

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)


###########################################################
# Calculation of 5th and 95th Percentiles and median, intercepte and slope of multitemporal statistics
###########################################################

func_5th_perc = function(x, na.rm = TRUE) {
    quantile(x, probs = .05, na.rm=TRUE)
}


func_95th_perc = function(x, na.rm = TRUE) {
    quantile(x, probs = .95, na.rm=TRUE)
}

func_median = function(x, na.rm = TRUE){median(x, na.rm = TRUE)}

# Calculation of slope and intercept in time series ----------------------------
# A much (> 100 times) faster approach is to directly use
# Linear algebra and pre-compute some constants

instances = 1:nlayers(file)

# add 1 for a model with an intercept
X <- cbind(1, instances)

# pre-computing constant part of least squares
invXtX <- solve(t(X) %*% X) %*% t(X)

# much reduced regression model; [2] is to get the slope
quickfun <- function(y) (invXtX %*% y)

# where [1] is intercept and [2] is slope


# # other regression models (slower)
#
# subset = vh[[1:4]]
#
# instances = 1:nlayers(subset)
# fun = function(x) {
#     lm(x ~ instances)
# }
#
# out = calc(subset, fun)


# Output ------------------------------------------------------------------------

# calulate 5th percentile
perc_5 = calc(file, fun=func_5th_perc, na.rm=TRUE)
writeRaster(perc_5, paste0("03_develop/multitemp/", filename, "_p05.tif"), overwrite = TRUE)

# calulate 95th percentile
perc_95 = calc(file, fun=func_95th_perc, na.rm=TRUE)
writeRaster(perc_95, paste0("03_develop/multitemp/", filename, "_p95.tif"), overwrite = TRUE)

# calculate median
med = calc(file, fun = func_median, na.rm = TRUE)
writeRaster(med, paste0("03_develop/multitemp/", filename, "_med.tif"), overwrite = TRUE)

out <- calc(file, quickfun)
writeRaster(out, paste0("03_develop/multitemp/", filename, "_linear.tif"), overwrite = TRUE)

# SAR Index --------------------------------------------------------------------

# batch calc median
map2(rasters, layernames) function(ras, name){
    med = calc(ras, fun = func_median, na.rm = TRUE)
    writeRaster(med, paste0("03_develop/multitemp/", name, "_med.tif"), overwrite = TRUE)
}
