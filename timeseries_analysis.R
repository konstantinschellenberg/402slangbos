# test set for calculation


source("import.R")

file = vh

###########################################################
# Calculation of 5th and 95th Percentiles and median
###########################################################

func_5th_perc = function(x, na.rm = TRUE) {
    quantile(x, probs = .05, na.rm=TRUE)
}


func_95th_perc = function(x, na.rm = TRUE) {
    quantile(x, probs = .95, na.rm=TRUE)
}

func_median = function(x, na.rm = TRUE){median(x, na.rm = TRUE)}

# calulate 5th percentile
perc_5 = calc(file, fun=func_5th_perc, na.rm=TRUE)
writeRaster(perc_5, paste0(path_developement, "multitemp/perc_5_test.tif"), overwrite = TRUE)

# calulate 95th percentile
perc_95 = calc(file, fun=func_95th_perc, na.rm=TRUE)
writeRaster(perc_95, paste0(path_developement, "multitemp/perc_95_test.tif"), overwrite = TRUE)

# calculate median
med = calc(file, fun = func_median, na.rm = TRUE)
writeRaster(med, paste0(path_developement, "multitemp/vh_med.tif"), overwrite = TRUE)

###########################################################
# Calculation of slope and intercept in time series
###########################################################
### A much (> 100 times) faster approach is to directly use
### linear algebra and pre-compute some constants

instances = 1:nlayers(file)

## add 1 for a model with an intercept
X <- cbind(1, instances)

## pre-computing constant part of least squares
invXtX <- solve(t(X) %*% X) %*% t(X)

## much reduced regression model; [2] is to get the slope
quickfun <- function(y) (invXtX %*% y)

# where [1] is intercept and [2] is slope
out <- calc(file, quickfun)
writeRaster(out, paste0(path_developement, "/multitemp/intercept_vh.tif"))

plot(out[[1]], main = "intercept")

# other regression models (slower)

subset = vh[[1:4]]

instances = 1:nlayers(subset)
fun = function(x) {
    lm(x ~ instances)
}

out = calc(subset, fun)


# SAR Index --------------------------------------------------------------------

# σ0 (dB) = 10*log10 (abs (σ0))
# Umkehrung: linear = 10^db

a = vv
b = vh

# ratio1 = log10(10^a / 10^b)
ratio = a - b
writeRaster(ratio, filename = paste0(path_s1, "vv_vh_ratio"), format = "ENVI", overwrite = T)

# Class stats ------------------------------------------------------------------
all_increase = grep2(gt_vh, 1)
