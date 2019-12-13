# Script for loading S1 data and parsing date from filename
# supported by Marcel Urban, University of Jena

# load required packages
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
library(latex2exp)

# Prerequisit:
# Sentinel-1 time-stack
# Polygons of the Region of interest with name == 1(slangbos encroachment) existing


################################################################################
# Import Sentinel-1 time series data--------------------------------------------
################################################################################

# for windows
s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"
# for linux
# s1_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/01_Daten/s1_data/S1_A_D_VH_free_state_study_area_geo402"
s1 = brick(s1vv_path)

# define nodata values
# file[file == -99] = NA

crs(s1)
ncell(s1)
dim(s1)
res(s1)
nlayers(s1)


################################################################################
# Import ROIs-------------------------------------------------------------------
################################################################################

roi_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"
# for linux
# roi_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/ROI_updated.kml"


roi_sf = st_read(roi_path) # read in

# set crs(roi) to the crs(s1) brick. Remove Z-Dimension
roi = st_transform(roi_sf, st_crs(s1)) %>%
    st_zm(drop = TRUE)

class(roi)
crs(roi)
# check if class == sf, crs == South African projection

################################################################################
# subset to the first ROI-------------------------------------------------------
################################################################################

# User decision which Roi with which code to use:
# 1 = Slangbos increase
# 2 = Slangbos decrease (burnt or ploughed)
# 12 = Slangbos increase with subsequent "2" event
# 3 = Continuous Slangbos coverage
# 4 = Agriculture


# Function definition
carve_brick = function(sentinel1_brick = s1,
                        polygon = roi,
                        code = 1,
                        roi_example_no = 1){

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
    ####

    # filtering code
    roi_code = filter(polygon, polygon$Name == code)

    # indexing single object
    single_roi = roi_code[roi_example_no, 1]

    # spatial subset with single ROI bounds
    subset = raster::extract(s1, single_roi) %>%
        as.data.frame()

    # convert band names to date
    bandnames = names(subset)

    # iterate for date in column-names
    for (i in bandnames){
        date = substr(bandnames,13,20)
    }

    # convert date string into R date-time format
    date_s1 = c()
    for (i in 1:length(date)){
        date_s1 <- append(date_s1, as.POSIXct(date[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }
    ####

    # integrate date to dataset: making time series
    # calculating the mean and margins (stdev), one transposition t() needed here
    df_date = subset %>%
        t() %>%
        as.data.frame() %>%
        mutate(date = date_s1) %>%
        na.omit()

    df = pivot_longer(df_date,
                      -date,
                      names_to = "names",
                      values_to = "values"
    )

    df_summary = df %>%
        group_by(date) %>%
        summarise(mean = mean(values),
                  median = median(values),
                  sd = sd(values),
                  "lower_sd" = mean(values) - sd(values),
                  "upper_sd" = mean(values) + sd(values))

    # printing summary to console
    paste = paste(
        paste("Size of the plot:", st_area(single_roi[1,]), sep = " "),
        paste("count of pixel in the timestack:", nrow(df), sep = " "),
        paste("ROI of type: ", codename, sep = " "),
        paste("median = ", mean(df_summary$median), sep = " "),
        paste("mean = ", mean(df_summary$mean), sep = " "),
        paste("standard deviation = ", mean(df_summary$sd), "\n", sep = " "),
        sep = "\n")
    ####

    cat(paste)
    return(df_summary)
}

raster_stats(sentinel1_brick = s1,
             polygon = roi,
             code = 2,
             roi_example_no = 1)

st_area(roi[1,])


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
     add = F
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

# making smoothed line for standard deviation
# pr_losd = supsmu(x, losd)
# pr_upsd = supsmu(x, upsd)

plt = plot_ly(data = df_summary,
              x = ~date,
              y = ~median,
              type = 'scatter',
              line = data.fmt,
              mode = "lines",
              name = paste(codename))

plt = plotly::layout(plt, title = paste0("Median Backscatter for site no. ",
                                         example_roi_no),
                     yaxis = list(range = c(-27, -10)))

plt = add_lines(plt,
                x = x,
                y = pr$y,
                line = line.fmt,
                name = "Gleitendes Mittel",
                )

# plt = add_lines(plt, x = x, y = pr_losd$y, line = interval.fmt, name = "Lower standard deviation")
# plt = add_lines(plt, x = x, y = pr_upsd$y, line = interval.fmt, name = "Upper standard deviation")
plt = add_ribbons(plt, x = x, ymin = losd, ymax = upsd, color = I("grey80"), line = list(width = 0), opacity = 0.9,
                  name = "1 sigma standard deviation")

print(plt)

# exporting graphics to dst
# plotly::orca(plt, "D:\\Geodaten\\#Jupiter\\GEO402\\work progress\\plotly")
