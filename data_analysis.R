###########################################################
# Calculation of 5th and 95th Percentiles
###########################################################

library(stars)

file = vh

# calculation of 5th percentile of data stack
# define function to calculate 5th percentile
func_5th_perc = function(x, na.rm=TRUE) {quantile(x, probs = c(.05), na.rm=TRUE)}

# calulate 5th percentile
perc_5 <- calc(file, fun=func_5th_perc, na.rm=TRUE)

# calculation of 95th percentile of data stack
# define function to calculate 95th percentile
func_95th_perc = function(x, na.rm=TRUE) {quantile(x, probs = c(.95), na.rm=TRUE)}

# calulate 95th percentile
perc_95 <- calc(file, fun=func_95th_perc, na.rm=TRUE)


# calc action
test = brick(vh[[1:5]])
test_stack = vh[[1:5]]

raster::inMemory(test)

perc_5 <- calc(test, fun=func_5th_perc, na.rm=TRUE)
plot(perc_5)
writeRaster(perc_5, paste0(path_developement, "multitemp/perc_5_test.tif"))

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

plot(out[[1]], main = "intercept")

