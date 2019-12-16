# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# This script serves to read Sentinel-1 Radar time series in order to extract region of interest (ROI)
# Functions are written to simplify the usage and enhance debugging
#
# 1. carve_brick:
# Reads s1 and roi, extracts "carves" the polygons in the raster brick and calculates some basic stats (mean, median, stdev)
#
# 2. list_summaries:
# Takes s1 and roi data, calls carve_brick and appends each retrieved dataframe-summary to a created list "summary"


# Prerequisits:
# Sentinel-1 time-stack
# Polygons of the region of interest (ROI) with the follwing column:
# name
# can have categories:
#     1 == slangbos encroachment, increase
#     12 == encroachment with cleaning/breakpoint
#     2 == cleaning/breakpoint
#     3 == continous persistance of slangbos
#     4 == agricultural site

# load required packages
library(raster)
library(dplyr)
library(rgdal)

library(tidyverse)
library(magrittr)
library(sf)
library(purrr)
library(plotly)
library(processx)
library(latex2exp)

################################################################################
# Import Sentinel-1 time series data--------------------------------------------
################################################################################

# windows
s1vv_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VV_free_state_study_area_geo402"
s1vh_path = "D:\\Geodaten\\#Jupiter\\GEO402\\01_data\\s1_data\\S1_A_D_VH_free_state_study_area_geo402"

# linux
# s1_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/01_Daten/s1_data/S1_A_D_VH_free_state_study_area_geo402"
s1vv = brick(s1vv_path)
s1vh = brick(s1vh_path)

crs(s1vv)
ncell(s1vv)
dim(s1vv)
res(s1vv)
nlayers(s1vv)


################################################################################
# Import ROIs-------------------------------------------------------------------
################################################################################

# windows
roi_path = "D:\\Geodaten\\#Jupiter\\GEO402\\02_features\\ROI_updated.kml"

# linux
# roi_path = "/home/aleko-kon/Dokumente/402_outdated/GEO402/ROI_updated.kml"

roi_sf = st_read(roi_path) # read in

# set crs(roi) to the crs(s1) brick. Remove Z-Dimension
roi = st_transform(roi_sf, st_crs(s1vv)) %>%
    st_zm(drop = TRUE)

class(roi)
crs(roi)
# check if class is sf, crs is South African projection

################################################################################
# Function definition `carve_brick`---------------------------------------------
################################################################################

carve_brick = function(sentinel1_brick,
                        polygon,
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
    subset = raster::extract(sentinel1_brick, single_roi) %>%
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

    datenames <<- date_s1

    # integrate date to dataset: making time series
    # calculating the mean and margins (stdev), one transposition t() needed here
    df_date = subset %>%
        t() %>%
        as.data.frame() %>%
        mutate(date = date_s1) #%>%
        # na.omit()

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
        paste("median = ", mean(df_summary$median, na.rm = TRUE), sep = " "),
        paste("mean = ", mean(df_summary$mean, na.rm = TRUE), sep = " "),
        paste("standard deviation = ", mean(df_summary$sd, na.rm = TRUE), "\n", sep = " "),
        sep = "\n")
    ####

    cat(paste)
    return(df_summary)
}

# how-to-call

df_summary = carve_brick(sentinel1_brick = s1vv,
                         polygon = roi,
                         code = 2,
                         roi_example_no = 1)


################################################################################
# Developing loop for `carve_brick`---------------------------------------------
################################################################################


list_summaries = function(sentinel1_brick, polygon){

    # help for data.frame in loops:
    # https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
    # set up function to set back all counters and variables before running the loops below
    initialise_counters = function(){
        code <<- 1
        ct_1 <<- 1
        ct_12 <<- 1
        ct_2 <<- 1
        ct_3 <<- 1
        ct_4 <<- 1
    }

    # run the intialisation
    initialise_counters()
    summary = list()
    counter = 1

    # for loop to write dataframes for each roi to the summary list
    for (code in roi$Name) {

        print(counter)

        if (code == 1) { #increase
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, roi_example_no = ct_1))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_1)
            ct_1 = ct_1 + 1

        } else if (code == 12) { #increase, then cleaned
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, roi_example_no = ct_12))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_12)
            ct_12 = ct_12 + 1

        } else if (code == 2) { #cleaning
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, roi_example_no = ct_2))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_2)
            ct_2 = ct_2 + 1

        } else if (code == 3) { #continuous
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, roi_example_no = ct_3))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_3)
            ct_3 = ct_3 + 1

        } else if (code == 4) { #agriculture
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, roi_example_no = ct_4))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_4)
            ct_4 = ct_4 + 1

        } else {
            print("Not correctly assigned ROI code")
        }
        counter = counter + 1
    }

    statement = paste(
        paste("Number of code 1:", ct_1),
        paste("Number of code 12:", ct_12),
        paste("Number of code 2:", ct_2),
        paste("Number of code 3:", ct_3),
        paste("Number of code 4:", ct_4),
        sep = "\n")

    # print statement to console
    cat(statement)

    return(summary)
}

 # how-to-call
# summary = list_summaries(sentinel1_brick = s1vh,
                         # polygon = roi)

################################################################################
# Data digesting----------------------------------------------------------------
################################################################################

# s1_vv_summaries
vv = list_summaries(sentinel1_brick = s1vv,
                                 polygon = roi)

# s1_vh_summaries
vh = list_summaries(sentinel1_brick = s1vh,
                                 polygon = roi)

# debug: check if different
identical(vv$plot1_1$median,
          vh$plot1_1$median)

################################################################################
# Calculations------------------------------------------------------------------
################################################################################
# UNDER CONSTRUCTION
class(vv)
class(vv$plot1_1)
class(vv$plot1_1$mean)


# initialise empty dataframe, nrow
# empty = data.frame(matrix(NA, nrow = length(sb_1$plot1_1$date)))



# Function definition-----------------------------------------------------------
summarise_roi = function(polarisation, category = "1", argument = "median"){
    # function to summaries the data with the given argument (must be a col name from carve_brick. E.g mean, median, sd)

    # http://www.endmemo.com/program/R/grepl.php ## regular expression syntax
    roi_vv = polarisation[grepl(paste0("plot", category, "_"), names(polarisation))]

    ####

    return(roi_vv)
}
summarise_roi(polarisation = vv,
              category = 2,
              argument = "median")

# ------------------------------------------------------------------------------

# medians transmuted
plot1_1 = sb_1$plot1_1

sb_1 = vv[grepl(paste0("plot", 1, "_"), names(vv))]



n = sb_1 %>%
    map_dfr("median") %>%
    mutate(date = datenames) %>%
    group_by(date)

n_2 = n %>%
    select(plot1_1, plot1_2) %>%
    summarise_all(mean)
n_2

    summarise_at(vars(1:5), mean, rm.na = TRUE)


n_2

n[grepl("plot", names(n))]

plot(t(n))
mutate(n, . ~mean)

# parse datenames

map(increase, mean)

################################################################################
# Plotting----------------------------------------------------------------------
################################################################################

# 1st Raster
mycolour = terrain.colors(20)

# raster
plot(s1[[1]],
     breaks = c(-25:-5),
     col = mycolour,
     axes = TRUE
)

# ROI
plot(roi[1], main = "All ROIs", col = "red", add = TRUE)

# Plotly graphs-----------------------------------------------------------------

vv_na = vv[["plot1_2"]]
vv_plot = na.omit(vv_na)

vh_na = vh[["plot1_2"]]
vh_plot = na.omit(vh_na)

# format edits
data.fmt = list(color="#878787", width=1)
line.fmt = list(dash="solid", width = 1.5, color="red")
# interval.fmt = list(dash="dot", width = 1, color="grey")


# save stats of vv and vh
x_vv = vv_plot$date
med_vv = vv_plot$median
losd_vv = vv_plot$lower_sd
upsd_vv = vv_plot$upper_sd

x_vh = vh_plot$date
med_vh = vh_plot$median
losd_vh = vh_plot$lower_sd
upsd_vh = vh_plot$upper_sd

# smoothing curve
pr_vv = supsmu(x_vv, med_vv)
pr_vh = supsmu(x_vh, med_vh)

# making smoothed line for standard deviation
# pr_losd = supsmu(x, losd)
# pr_upsd = supsmu(x, upsd)

plt = plot_ly(data = vv_plot,
              x = ~date,
              y = ~median,
              type = 'scatter',
              line = data.fmt,
              mode = "lines")
              # name = paste(codename))

plt = plotly::layout(plt, title = paste0("Median Backscatter for site no. ", 1),
                     yaxis = list(range = c(-27, -8)))

plt = add_lines(plt,
                x = x,
                y = pr_vv$y,
                line = line.fmt,
                name = "Smooth VV",
                )

# plt = add_lines(plt, x = x, y = pr_losd$y, line = interval.fmt, name = "Lower standard deviation")
# plt = add_lines(plt, x = x, y = pr_upsd$y, line = interval.fmt, name = "Upper standard deviation")
plt = add_ribbons(plt, x = x, ymin = losd_vv, ymax = upsd_vv,
                  color = I("grey80"), line = list(width = 0), opacity = 0.9,
                  name = "VV 1 sigma standard deviation")

## add vh
plt = add_lines(plt,
                x = x,
                y = med_vh,
                name = "VH")

plt = add_lines(plt,
                x = x,
                y = pr_vh$y,
                line = line.fmt,
                name = "Smooth VH")

plt = add_ribbons(plt, x = x, ymin = losd_vh, ymax = upsd_vh,
                  color = I("grey80"), line = list(width = 0), opacity = 0.9,
                  name = "VH 1 sigma standard deviation")


print(plt)

# exporting graphics to dst
# plotly::orca(plt, "D:\\Geodaten\\#Jupiter\\GEO402\\work progress\\plotly")
