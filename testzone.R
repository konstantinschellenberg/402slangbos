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
##################################################################################

# try writing function
# import data
s1_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"
s1 = brick(s1_path)

roi_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"
roi_sp = readOGR(roi_path, "ROI_updated")
roi = roi_sp %>%
    st_as_sf() %>%
    st_transform(st_crs(s1))

# ------------------------------------------------------------------------------

#write band names
bandnames = names(s1)

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

# ------------------------------------------------------------------------------

# store code as dataframe column-name
# receiving code

roi_increase = roi %>%
    filter(roi$Name == code)

# get single object
example_roi = roi_increase[example_roi_no, 1]

# make spatial subset with ROI bounds
subset = raster::extract(s1, roi[1,])

# convert to dataframe
subset_df = as.data.frame(subset)

subset = s1 %>%
    raster::extract(roi[1,]) %>%
    as.data.frame()


frame = data.frame()
subsetting = function(roi, brick){

    list_of_df = list()

    for (roi in roi) {

        # retrieve colname for colnames in final df
        colname = roi$Name

        df = s1 %>%
            raster::extract(roi) %>%
            as.data.frame()

        list_of_df = c(list_of_df, df)
    }
    return(list_of_df)
}


# run function
subsetting(roi, s1)

roi

for (i in roi){
    print(i)
    print(class(i))
    colname = i$Name
    return(colname)
}

#
# for (i in roi) {
#     if (i[1]) {
#         columnname = roi$Name
#     }
# }
#
# columnname

a = list(data.frame(), data.frame())


# R using c function to append values to list
mylist <- c(1,2,3,4)
newelem <- 5
mylist <- c(mylist, newelem)
mylist

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# DEPRECATED CODE; NOW ALL IN FUNCTION------------------------------------------

roi_code = filter(roi, roi$Name == code)

example_roi_no = 1
code = 1

codename = ""

# looping for plotting name
if (code == 1) {
    codename = "Increase"
}
if (code == 2) {
    codename = "Cleaned"
}
if (code == 12) {
    codename = "Increase, then cleaned"
}
if (code == 3) {
    codename = "Continuous"
}
if (code == 4) {
    codename = "Agriculture"
}

# receiving code
roi_increase = roi %>%
    filter(roi$Name == code)

# get single object
example_roi = roi_increase[example_roi_no, 1]

# make spatial subset with ROI bounds
subset = raster::extract(s1, example_roi)

# convert to dataframe
subset_df = as.data.frame(subset)

# convert band names to date
bandnames = names(subset_df)

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

# ------------------------------------------------------------------------------

# integrate date to dataset: making time series
# hereby the table needs to be transposed temporarily as `xts()` orders by rows
# calculating the mean and margins (stdev), one transposition needed here (better this way?)
df_date = subset_df %>%
    t() %>%
    as.data.frame() %>%
    mutate(date = date_s1) %>%
    na.omit()

df = pivot_longer(df_date,
                  -date,
                  names_to = "names",
                  values_to = "values"
)

paste("count of pixel in the timestack:", nrow(df))

df_summary = df %>%
    group_by(date) %>%
    summarise(mean = mean(values),
              median = median(values),
              sd = sd(values),
              "lower_sd" = mean(values) - sd(values),
              "upper_sd" = mean(values) + sd(values))


# map of df_list

