# code from top to parsing file name by Marcel Urban 28.11.2019, University of Jena
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

###########################################################
# Import Sentinel-1 time series data
###########################################################

in.name = "/home/aleko-kon/Dokumente/402_outdated/GEO402/01_Daten/s1_data/S1_A_D_VH_free_state_study_area_geo402"

file = brick(in.name)

#define nodata values
file[file == -99] = NA

#convert from dB to linear values
file = 10^(file/10)

#save as data frame without spatial information
df = as.data.frame(file) #xy=TRUE


#extract date
# convert band names to date

bandnames = names(df)

for (i in bandnames){
        date = substr(bandnames,13,20)
}


# convert date string into R date-time format
date_s1 = c()
for (i in 1:length(date)){
        date_s1 <- append(date_s1, as.POSIXct(date[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }
