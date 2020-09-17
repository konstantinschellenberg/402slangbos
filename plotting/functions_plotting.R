#' Revisited version of plotting, simplified
#' Konstantin Schellenberg, 1.4.2020
#'
#' Example plots:
#' 1,7
#' 1,12
#' 1,16
#' 1,26
#' 1,32
#'
#' 2,2x
#' 2,6x
#' 2,8
#' 2,17
#' 2,22
#' 2,31
#' 2,32
#'
#' 3,2
#' 3,4
#'
#' 4,2
#' 4,4
#'
#' 6,1
#' 6,2
#'
#' 7,5
#' 7,9

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(mapview)
library(ggplot2)

options(max.print = 100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)


# Import files -----------------------------------------------------------------
# create these file in script mlr3_preprocessing

# files in directory
gt.files = list.files("D:/Geodaten/#Jupiter/GEO402/03_develop/extract/", pattern = "extract", full.names = TRUE)

types = map(gt.files, ~ substr(.x, start = 56, stop = 100))

dfs = map(gt.files, ~ readRDS(.x)) %>% `names<-`(types)


# example stats
ex = stats(dfs, 1, 15, coherence_smoothing = F)



## SOLVE NDVI PROBLEM! BZW: WHAT TO DO WITH UPPER LIST
# # calc NDVI
# ndvi.pre.red = grep2(gt_red, c, n)
# ndvi.pre.nir = grep2(gt_nir, c, n)
#
# ndvi = data.frame(matrix(nrow = nrow(list$nir), ncol = 2))
# ndvi$X2 = fun.ndvi(list$red$median, list$nir$median)
# ndvi$X1 = list$red$date
# names(ndvi) = c("date", "ndvi")
#
# # optional scaling of the ndvi (only for comparison purposes, scale on stdev)
# # ndvi$X2 = scale(ndvi$X2, center = T) %>% as.vector()
#
# list = list.append(list, ndvi = ndvi)
# return(list)


# User choice (which is filed to be analysed) ----------------------------------

# class, number
c = 4
n = 2

# Map --------------------------------------------------------------------------

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


map = function(c, n){
    # load classificatin palette
    qgis_pal = get_qgis_palette("D:/Geodaten/#Jupiter/GEO402/layout/classification_palette.csv")

    # get geographical position and identify of reference plot
    sf = position(c, n)

    description = data.frame(asdf = "Increase", "Continuous", "Breakpoint", "Agriculture", "Bare Soil", "Grassland", "Forest", "Urban", "Water")

    # aggregate overview median raster
    r = raster::aggregate(vh_med, fact = 2, fun = mean)

    # optional: classification
    # pred.classif = pred[[3]]
    # pred.classif = raster::aggregate(pred.classif, fact = 2, fun = modal)
    # pred.classif = pred[[3]]
    # vrn = raster::aggregate(pred[[3]], fact = 2, fun = max)

    mypopup = paste0("ID: ", gt$descrip, "<br>", "Number: ", gt$number, "<br> Area: ",
                     round(st_area(gt)), " m²")

    rasterpal = colorNumeric(c("black", "white"), values(r), na.color = "transparent")
    factpal = colorFactor(palette = qgis_pal, domain = gt$Name)
    # classifpal = colorFactor(palette = qgis_pal, domain = 1:9, na.color = "transparent")

    # if zoomed to gt example
    lon = st_bbox(st_centroid(sf))[1] %>% as.vector()
    lat = st_bbox(st_centroid(sf))[2] %>% as.vector()

    mymap = leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(x = r, opacity = 0.9, colors = rasterpal, group = "VH Median") %>%
        # addRasterImage(x = pred.classif, opacity = 0.9, colors = classifpal, group = "Classification") %>%
        setView(lon, lat, zoom = 17) %>%
        addPolygons(data = st_transform(study_area, 4326),
                    fillOpacity = 0,
                    weight = 3,
                    color = "black") %>%
        addPolygons(data = st_transform(gt, 4326),
                    fillColor = ~factpal(Name),
                    color = ~factpal(Name), # you need to use hex colors
                    fillOpacity = 0,
                    weight = 3,
                    smoothFactor = 0.1,
                    popup = mypopup, group = "Reference plots") %>%
        # addLegend(position = "bottomright", pal = rasterpal, values = values(r), title = "S-1 VH backscatter,<br><sub>5-year median</sub>",
        #           labFormat = labelFormat(suffix = " [db]")) %>%
        addLegend(position = "bottomright", colors = qgis_pal, values = gt$Name, title = "Classes",
                  labels = c("Increase", "Continuous", "Breakpoint", "Agriculture", "Bare Soil", "Grassland", "Forest", "Urban", "Water")) %>%
        addLayersControl(
            overlayGroups = c("VH Median", "Reference plots"),
            options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("VH Median"))
    return(mymap)
}

# map(c, n)

# save leaflets
# mapview::mapshot(map(a, i), file = paste0("hiwi/", a, "/map_", a, "-", i, ".png"), remove_controls = c("zoomControl", "layersControl"))


# Plot 1 ------------------------------------------------------------------------
# Comparison VH backscatter, VV coherence, NDVI

plt1 = function(a, all = TRUE, title = NULL){

    # save map

    if (all == TRUE){
        co_data = a$covv_all
    } else {
        co_data = a$covv
        }

    plt = plot_ly(width = 700, height = 500) %>%

        ### LINES ------------------------------------------------------------------
    # VH
    add_lines(data = a$vh, x = ~date, y = ~med_smooth,
              yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%

    # VV coherence
    add_lines(data = co_data, x = ~date, y = ~med_smooth,
              yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
              connectgaps = F) %>%

    # NDVI
    # add_lines(data = a$ndvi, x = ~date, y = ~ndvi,
    #           yaxis = "y3", name = "NDVI", line = ndvi.fmt) %>%

        ### MARKERS-----------------------------------------------------------------

    add_lines(data = a$covv_all, x = ~date, y = ~median,
              yaxis = "y2", name = "S-1 VV coherence (2-weeks interval)",
              marker = list(size = 3, color = "orange"), line = red2.fmt,
              connectgaps = F) %>%

        ### RIBBONS ----------------------------------------------------------------
    add_ribbons(data = a$vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                color = I(blue_background), line = list(width = 1), opacity = 0.3,
                showlegend = T) %>%

    add_ribbons(data = co_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y2", name = "S-1 VV coherence standard deviation range (1 sigma)",
                color = I(grey_background), line = list(width = 1), opacity = 0.3,
                showlegend = T) %>%

    layout(xaxis = x, yaxis = y.s1, yaxis2 = y.co_2,
           # yaxis3 = y.s2_2, # NDVI Axis
           legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
           margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
           title = title)

    return(plt)
}


# Plot 2 -----------------------------------------------------------------------
# Comparison VV, VH backscatter

plt2 = function(a, scatter = TRUE, title = NULL){

    plt = plot_ly(width = 700, height = 500) %>%

    ### SMOOTH LINES ------------------------------------------------------------------
    # VH
    add_lines(data = a$vh, x = ~date, y = ~med_smooth,
              yaxis = "y1", name = "S-1 VH backscatter smoothed (median of pixels)", line = vh.fmt) %>%

    # VV
    add_lines(data = a$vv, x = ~date, y = ~med_smooth,
              yaxis = "y1", name = "S-1 VV backscatter smoothed (median of pixels)", line = black.fmt)

    if (scatter == TRUE){

        ### MARKERS -----------------------------------------------------------------
        plt = add_lines(plt, data = a$vh, x = ~date, y = ~median,
                  yaxis = "y1", name = "S-1 VH backscatter (median of pixels)",
                  line = vh.fmt.slim,
                  connectgaps = F) %>%

        add_lines(data = a$vv, x = ~date, y = ~median,
                  yaxis = "y1", name = "S-1 VV backscatter (median of pixels)",
                  line = black.fmt.slim,
                  connectgaps = F)
    }

    ### RIBBONS ----------------------------------------------------------------
    plt = add_ribbons(plt, data = a$vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                color = I(blue_background), line = list(width = 1), opacity = 0.3,
                showlegend = TRUE) %>%

    add_ribbons(data = a$vv, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y1", name = "S-1 VV standard deviation range (1 sigma)",
                color = I(grey_background), line = list(width = 1), opacity = 0.3,
                showlegend = TRUE) %>%

    layout(xaxis = x, yaxis = y.s1.general,
           legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -1, x = 0.5),
           margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
           title = title)

    return(plt)
}


# Plot 3 -----------------------------------------------------------------------
# Comparison VV, VH Coherences

# only available when VH coherences have been derived
plt3 = function(a, all = TRUE, title = NULL){


    if (all == TRUE){
        covv_data = a$covv_all
        covh_data = a$covh_all
    } else {
        covv_data = a$covv
        covh_data = a$covh
        }

    plt = plot_ly(width = 700, height = 500) %>%

        # lines
        add_lines(data = covv_data, x = ~date, y = ~med_smooth,
                  name = "S1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                  connectgaps = F) %>%
        add_lines(data = covh_data, x = ~date, y = ~med_smooth,
                  name = "S1 VH coherence smoothed (2-weeks interval)", line = nir.fmt,
                  connectgaps = F) %>%

        # markers
        add_lines(data = a$covv_all, x = ~date, y = ~median,
                  name = "S1 VV coherence (2-weeks interval)",
                  marker = list(size = 3, color = "orange"), line = red2.fmt,
                  connectgaps = F) %>%
        add_lines(data = a$covh_all, x = ~date, y = ~median,
                  name = "S1 VH coherence (2-weeks interval)",
                  marker = list(size = 3, color = "green"), line = nir2.fmt,
                  connectgaps = F) %>%

        # ribbons
        add_ribbons(data = covv_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                    name = "VH standard deviation range [1 sigma]",
                    color = I(grey_background), line = list(width = 1), opacity = 0.3,
                    showlegend = FALSE) %>%
        add_ribbons(data = covh_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                    name = "VH standard deviation range [1 sigma]",
                    color = I(green_background), line = list(width = 1), opacity = 0.2,
                    showlegend = FALSE) %>%

        layout(xaxis = x, yaxis = y.co,
               legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
               margin = list(pad = 10, b = 200, l = 80, r = 80, automargin = TRUE),
               title = title)
    return(plt)
}

# Plot 4 -----------------------------------------------------------------------
# Comparison VV backscatter, VV coherence, NDVI

plt4 = function(a, all = TRUE, title = NULL){


    if (all == TRUE){
        co_data = a$covv_all
    } else {
        co_data = a$covv
    }

    plt = plot_ly(width = 700, height = 500) %>%

        ### LINES ------------------------------------------------------------------
    # VV
    add_lines(data = a$vv, x = ~date, y = ~med_smooth,
              yaxis = "y1", name = "S-1 VV backscatter smoothed", line = vv.fmt) %>%

    # VV coherence
    add_lines(data = co_data, x = ~date, y = ~med_smooth,
              yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
              connectgaps = F) %>%

    # NDVI
    # add_lines(data = a$ndvi, x = ~date, y = ~ndvi,
    #           yaxis = "y3", name = "NDVI", line = ndvi.fmt) %>%

        ### MARKERS-----------------------------------------------------------------

    add_lines(data = a$covv_all, x = ~date, y = ~median,
              yaxis = "y2", name = "S-1 VV coherence (2-weeks interval)",
              marker = list(size = 3, color = "orange"), line = red2.fmt,
              connectgaps = F) %>%

        ### RIBBONS ----------------------------------------------------------------
    add_ribbons(data = a$vv, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y1", name = "S-1 VV standard deviation range (1 sigma)",
                color = I(green_background), line = list(width = 1), opacity = 0.15,
                showlegend = T) %>%

    add_ribbons(data = co_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                yaxis = "y2", name = "S-1 VV coherence standard deviation range (1 sigma)",
                color = I(grey_background), line = list(width = 1), opacity = 0.3,
                showlegend = T) %>%

    layout(xaxis = x, yaxis = y.s1.vv, yaxis2 = y.co_2,
           # yaxis3 = y.s2_2, # NDVI Axis
           legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
           margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
           title = title)

    return(plt)
}
# CALL PLOTS -------------------------------------------------------------------
#' Example plots:
#' 1,7
#' 1,12
#' 1,16
#' 1,26
#' 1,32
#'
#' 2,2x
#' 2,6x
#' 2,8
#' 2,17
#' 2,22
#' 2,31
#' 2,32
#'
#' 3,2
#' 3,4
#'
#' 4,2
#' 4,4
#'
#' 6,1
#' 6,2
#'
#' 7,5
#' 7,9

y.s1.vv <- list(
    title = list(text = "S1 VV backscatter [dB]", standoff = 50),
    titlefont = f2,
    tickfont = f1,
    showline = F,
    showgrid = FALSE,
    anchor = "free",
    position = 0)


setwd(env)

