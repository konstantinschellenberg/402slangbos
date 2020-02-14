# Useful methods to plot raster data
# https://oscarperpinan.github.io/rastervis/

source("import.R")

a = levelplot(vv, layers = 1, margin = list(FUN = 'median'), contour=TRUE)

a + layer(gt[0])
