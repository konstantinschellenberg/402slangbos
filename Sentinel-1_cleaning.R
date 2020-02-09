# Sentinel-1 cleaning

source("import.R")

vv1 = vv[[1]]
vh1 = vh[[1]]

ratio2 = system.time(raster::calc(vv1, fun = x - vh1))

# batch calculation
ratio_brick = vv - vh
