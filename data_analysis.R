###########################################################
# Calculation of 5th and 95th Percentiles
###########################################################

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
