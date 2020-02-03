library(raster)
library(dplyr)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(rts)
library(sf)
library(purrr)
library(plotly)
library(processx)
# library(openair) # Marcel packages
# library(readxl)
# library(scales)
# library(cowplot)

f <- system.file("external/rlogo.grd", package="raster")
f

r1 = raster(f)
r1

r2 = raster(f, band=2)
r2
plot(r1)

b = brick(f)
plot(b)

writeRaster(b, "\\ignored\\output.tif")

# reading
pathname = "output.tif"

b = brick(pathname)
# writing
writeRaster(b, "\\ignored\\output_overwritten.tif", overwrite = TRUE)

# ------------------------------------------------------------------------------

#convert from dB to linear values, already done.
file = 10^(file/10)

# subsetting in two halves
half1 = subset(file, 1 : (n/2))

half2 = subset(file, (n/2+1) : n)

#save as data frame without spatial information
# df = as.data.frame(a) #xy=TRUE
# ------------------------------------------------------------------------------

library(zoo)

z <- read.zoo(text = Lines, header = TRUE, format = "%m/%d/%Y")
#
# ------------------------------------------------------------------------------

a = letters[1:5]

map_chr(a, ~ "z")
a %>%
    map(3)

# --------------------------------------------------------------------------------
# Testing CAST------------------------------------------------------------------

library("CAST")

data <- get(load(system.file("extdata","Cookfarm.RData",package="CAST")))
head(data)
names(data)

library(lubridate)
library(ggplot2)
trainDat <- data[data$altitude==-0.3&
                     year(data$Date)==2012&
                     week(data$Date)%in%c(13:14),]
ggplot(data = trainDat, aes(x=Date, y=VW)) +
    geom_line(aes(colour=SOURCEID))

library(caret)
predictors <- c("DEM","BLD","TWI","Precip_cum","cday",
                "MaxT_wrcc","Precip_wrcc",
                "Northing","Easting","NDRE.M")
set.seed(10)
model <- train(trainDat[,predictors],trainDat$VW,
               method="rf",tuneLength=1,importance=TRUE,
               trControl=trainControl(method="cv",number=5))
# ------------------------------------------------------------------------------
# Import dbf
library(foreign)
library(tidyverse)

dbf = read.csv("D:/Geodaten/#Jupiter/GEO402/03_develop/k-means/1b_stats.dbf", sep = "")
dbf

ras = read_stars("D:/Geodaten/#Jupiter/GEO402/03_develop/k-means/1b_cluster.sdat", proxy = T)
plot(ras, col = viridis::viridis(10))

ras = raster("D:/Geodaten/#Jupiter/GEO402/03_develop/k-means/1b_cluster.sdat")
plot(ras[1])
