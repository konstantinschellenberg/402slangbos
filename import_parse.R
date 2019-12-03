# Script for loading S1 data and parsing date from filename
# supported by Marcel Urban, University of Jena

# load required packages
library(raster)
library(dplyr)
library(rgdal)
library(ggplot2)
# library(openair)
# library(readxl)
# library(scales)
# library(cowplot)

library(tidyverse)
library(magrittr)
library(rts)
library(sf)
library(purrr)
library(plotly)

# Prerequisit:
# Sentinel-1 time-stack
# Polygons of the Region of interest with name == 1(slangbos encroachment) existing


###########################################################
# Import Sentinel-1 time series data
###########################################################

# for windows
s1_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"
# for linux
# s1_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/01_Daten/s1_data/S1_A_D_VH_free_state_study_area_geo402"
s1 = brick(s1_path)

# define nodata values
# file[file == -99] = NA

crs(s1)
ncell(s1)
dim(s1)
res(s1)
nlayers(s1)

###########################################################
# Import ROIs
###########################################################

roi_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"
# for linux
# roi_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/ROI_updated.kml"

# read in
roi_sp = readOGR(roi_path, "ROI_updated")

# convert to sf object
# set crs of s1 layer
roi = roi_sp %>%
    st_as_sf() %>%
    st_transform(st_crs(s1))

# check if class == sf, crs == South African projection
class(roi)
crs(roi)

# --------------------subset to the first ROI-----------------------------------

# User decision which Roi with which code to use:
# 1 = Slangbos increase
# 2 = Slangbos decrease (burnt or ploughed)
# 12 = Slangbos increase with subsequent "2" event
# 3 = Continuous Slangbos coverage
# 4 = Agriculture

example_roi_no = 1
code = 1

# get object with name = 1
roi_increase = roi %>%
    filter(roi$Name == code)

example_roi = roi_increase[example_roi_no, 1]


# here: iteration through polygons and writing to df ###############
# frame = data.frame(a = 1:10,b= 1:10)
#
# for (i in nrow(roi)) {
#     # extract pixels from subset
#     subset = as.data.frame(raster::extract(s1, i))
#
#     # create dataframe
#     frame %<>%
#         cbind(datasubset)
#         #add col
#
#     # add dataframe to frame as nested dataframe
# }


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


# -------------------PLOTTING---------------------------------------------------


# 1st Raster
mycolour = terrain.colors(6)

# plot(s1[[1]],
#      breaks = c(-30, -25, -20, -15, -10, -5),
#      col = mycolour,
#      axes = TRUE
# )

# all rois
plot(roi[1], main = "All ROIs", col = "red")

# example roi
plot(example_roi,
     main = "Example Used",
     col = "black",
     axes = TRUE,
     add = T
)

# format edits
data.fmt = list(color="#878787", width=1)
line.fmt = list(dash="solid", width = 1.5, color="red")
interval.fmt = list(dash="dot", width = 1, color="grey")

x = df_summary$date
med = df_summary$median
losd = df_summary$lower_sd
upsd = df_summary$upper_sd

pr = supsmu(x, med)
pr_losd = supsmu(x, losd)
pr_upsd = supsmu(x, upsd)

plt = plot_ly(data = df_summary,
              x = ~date,
              y = ~median,
              type = 'scatter',
              line = data.fmt,
              mode = "lines",
              name = paste(code))

plt = plotly::layout(plt, title = paste0("Median Backscatter for site no. ", example_roi_no))

plt = add_lines(plt,
                x = x,
                y = pr$y,
                line = line.fmt,
                name = "Gleitendes Mittel",
                )
# plt = add_lines(plt, x = x, y = pr_losd$y, line = interval.fmt, name = "Lower standard deviation")
# plt = add_lines(plt, x = x, y = pr_upsd$y, line = interval.fmt, name = "Upper standard deviation")
plt = add_ribbons(plt, x = x, ymin = pr_losd$y, ymax = pr_upsd$y, color = I("grey80"), line = list(width = 0), opacity = 0.9, name = "Interval")

print(plt)

interval_ribbon.fmt
