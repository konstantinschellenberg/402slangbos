# Script for loading S1 data and parsing date from filename
# supported by Marcel Urban, University of Jena

# load required packages
library(raster)
library(dplyr)
library(reshape2)
library(rgdal)
library(ggplot2)
library(openair)
library(readxl)
library(scales)
library(cowplot)

library(rts)
library(sf)
library(dplyr)

###########################################################
# Import Sentinel-1 time series data
###########################################################

s1_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"

s1 = brick(s1_path)

# define nodata values
# file[file == -99] = NA

crs(s1)
ncell(s1)
dim(s1)
res(s1)
n = nlayers(s1)

###########################################################
# Import ROIs
###########################################################

roi_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"

# make spatial subset with ROI bounds
roi = readOGR(roi_path, "ROI_updated")

class(roi)
crs(roi)
plot(roi, axes = TRUE, col="blue")

#convert to sf object (S4)
sf_roi = st_as_sf(roi)


# set crs of s1 layer
st_transform(sf_roi, st_crs(s1))

# --------------------subset to the first ROI-----------------------------------

# get object with name = 1
roi_increase = sf_roi %>%
    filter(sf_roi$Name == 1)

test_polygon = roi_increase[1,1]

plot(test_polygon, main = "1")

# ------------------------------------------------------------------------------

# here: iteration through polygons and writing to df

subset = extract(s1, roi_1[1,1])

# convert to dataframe
df = as.data.frame(subset)

# convert band names to date
bandnames = names(df)

# iterate for date in column-names
for (i in bandnames){
    date = substr(bandnames,13,20)
}

# convert date string into R date-time format
date_s1 = c()
for (i in 1:length(date)){
    date_s1 <- append(date_s1, as.POSIXct(date[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
}
date_s1

# replace colnames by date
#
# fstd = date_s1[1]
#
# names(df)[1] = fstd
# colnames(df)[1]

# ------------------------------------------------------------------------------
# Baustelle --------------------------------------------------------------------

ts = rts(s1, date_s1)

test_sp = as(test_polygon, "Spatial")
class(test_sp)
plot(test_sp)
crs(test_sp)

# extracting polygon from time series
ex = extract(ts, test_sp)
plot(ex)
class(ex)
ex[1]


ggplot(data = ex, aes(x = DATE, y = PRECIP)) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = "Total daily precipitation in Boulder, Colorado",
         subtitle = "Fall 2013",
         x = "Date", y = "Daily Precipitation (Inches)")

