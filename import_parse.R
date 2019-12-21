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

    # built-in function for naming rois
    namer <<- function(code){

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
    return(codename)
    }

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
        paste("ROI of type: ", namer(code), sep = " "),
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
