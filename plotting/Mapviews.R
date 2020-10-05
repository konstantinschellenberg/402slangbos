# Mapview

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
library(data.table)
library(mapview)
library(leaflet)
library(leafem)
library(jsonlite)

library(exactextractr)

options(max.print=100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")


# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
plotdir = "06_plots/"
destdir = "03_develop/extract/"

# READ IN MASTER DATA ----------------------------------------------------------

master = readRDS("03_develop/extract/extract_all.RDS")

# Map Function

# read in QGIS palette
get_qgis_palette = function(csv){
    q_pal.in = read.csv(csv, header = F, sep = " ")
    q_pal = q_pal.in[,2:4]
    pal = c()
    for (i in 1:dim(q_pal)[1]){ # row length
        pal = append(pal, values = rgb(red = q_pal[i,1], green = q_pal[i,2], blue = q_pal[i,3], maxColorValue = 255))
    }
    return(pal)
}

# ras_example = raster("03_develop/multitemp/vh_med.tif") %>% raster::aggregate(fact = 1, fun = mean)

mapping = function(class, sample, raster = NULL){

    if (!is.null(raster)){
        rasterpal = colorQuantile(c("white", "black"), values(raster), na.color = "transparent")
    }
    # Custom palette
    qgis_pal = get_qgis_palette("D:/Geodaten/#Jupiter/GEO402/layout/classification_palette.csv") %>% .[1:5]

    # get geographical position and identify of reference plot
    sf = gt %>% position(class, sample) %>%
        st_transform(4326)

    mypopup = paste0("Class:", gt$new_class, "<br>", "Date of Clearning: ", paste0(substring(gt$break_date, 1, 2), "-", substring(gt$break_date, 3, 6)), "<br>",
                     "ID: ", gt$checked_name, "<br>", "Sample: ", gt$id, "<br> Area: ",
                     round(st_area(gt)), " m²")

    factpal = colorFactor(palette = qgis_pal, domain = gt$class_simple)

    # if zoomed to gt example
    lon = st_bbox(st_centroid(sf))[1] %>% as.vector()
    lat = st_bbox(st_centroid(sf))[2] %>% as.vector()

    if (is.null(raster)){
        mymap = leaflet() %>%
            addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
            addProviderTiles("Stamen.TonerHybrid", group = "Stamen.TonerHybrid") %>%
            setView(lon, lat, zoom = 15) %>%
            addPolygons(data = st_transform(study_area, 4326),
                        fillOpacity = 0,
                        weight = 3,
                        color = "black") %>%
            addPolygons(data = st_transform(gt, 4326),
                        fillColor = ~factpal(class_simple),
                        color = ~factpal(class_simple), # you need to use hex colors
                        fillOpacity = 1,
                        weight = 3,
                        smoothFactor = 0.5,
                        popup = mypopup, group = "Reference Sites") %>%
            addLegend(position = "bottomright", colors = qgis_pal, values = gt$class_simple, title = "Classes",
                      labels = classnames) %>%
            addLayersControl(
                overlayGroups = c("Reference Sites", "Esri.WorldImagery", "Stamen.TonerHybrid"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup(c("VH Median")) %>%
            addMouseCoordinates()
    }

    if (!is.null(raster)){
        mymap = leaflet() %>%
            addProviderTiles("Esri.WorldImagery",  group = "Esri.WorldImagery") %>%
            addProviderTiles("Stamen.TonerHybrid", group = "Stamen.TonerHybrid") %>%
            addRasterImage(x = raster, opacity = 0.9, colors = rasterpal, group = "VH Median") %>%
            setView(lon, lat, zoom = 15) %>%
            addPolygons(data = st_transform(study_area, 4326),
                        fillOpacity = 0,
                        weight = 3,
                        color = "black") %>%
            addPolygons(data = st_transform(gt, 4326),
                        fillColor = ~factpal(class_simple),
                        color = ~factpal(class_simple), # you need to use hex colors
                        fillOpacity = 1,
                        weight = 3,
                        smoothFactor = 0.5,
                        popup = mypopup, group = "Reference Sites") %>%
            addLegend(position = "bottomright", colors = qgis_pal, values = gt$class_simple, title = "Classes",
                      labels = classnames) %>%
            addLayersControl(
                overlayGroups = c("VH Median", "Reference Sites", "Esri.WorldImagery", "Stamen.TonerHybrid"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup("VH Median") %>%
            addMouseCoordinates()
    }
    return(mymap)
}


# mapping(3,15)
# mapping(1, 1, ras_example)

# position(gt, class_nr = 1, sample_nr = 1)
