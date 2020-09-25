# PLotting summary statistics

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)

library(exactextractr)

options(max.print=100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
proper_layernames = c("Sentinel-1 VH", "Sentinel-1 VV", "Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI",
                       "Sentinel-2 MSAVI", "Sentinel-2 NDVI", "Sentinel-2 REIP", "Sentinel-2 RVI")
proper_layernames.axis = c("S-1 VH [dB]", "S-1 VV [dB]", "S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index",
                           "S-2 MSAVI Index", "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
plotdir = "06_plots/"

# READ IN ----------------------------------------------------------------------
data = readRDS(paste0(dstdir, "summary_statistics.RDS"))

# clean data from NA (possible step in between)
summary = map(data, ~ map(.x, function(x){
    na.omit(x)
})
)

# INDIZES DEFINITION -----------------------------------------------------------

# give named list of indizes
indizes = list(evi = summary[["evi"]], ndvi = summary[["ndvi"]], reip = summary[["reip"]],
               msavi = summary[["msavi"]], dvi = summary[["dvi"]], rvi = summary[["rvi"]])
i_name = names(indizes)

# range berechnen
ranges = map(indizes, function(x){
    max = map_dbl(x, ~ max(.x$upsd_smooth)) %>% max()
    min = map_dbl(x, ~ min(.x$losd_smooth)) %>% min()
    range = list(c(min, max))
})

ranges.all = map(summary, function(x){
    max = map_dbl(x, ~ max(.x$upsd_smooth)) %>% max()
    min = map_dbl(x, ~ min(.x$losd_smooth)) %>% min()
    range = list(c(min, max))
})

# define normalised indizes manually
# ranges$evi[[1]] = c(0, 1); ranges$ndvi[[1]] = c(0, 1)

################################################################################
# GGPLOTS SIMPLE ---------------------------------------------------------------
p.vh = summary[[1]]
p.vv = summary[[2]]
p.co = summary[[3]]
p.ndvi = summary[["ndvi"]]

# Slangbos increase VH
ggplot(p.vh[[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth))

# Slangbos increase COH
ggplot(p.co[[1]]) +
    geom_point(aes(date, median)) +
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth))

# Slangbos increase NDVI
map(p.ndvi, ~ ggplot(.x) +
    geom_point(aes(date, median))+
    geom_line(aes(date, median)) +
    geom_line(aes(date, med_smooth)))

ggplot(p.co[[2]], aes(x = date)) +
    geom_line(aes(y = median)) +
    geom_line(aes(y = med_smooth)) +
    geom_ribbon(aes(ymin = losd_smooth, ymax = upsd_smooth), fill = "grey", alpha = 0.5) +
    theme_minimal()

arr = list()
for (i in seq_along(rasters)){
    summary_of_raster = summary[[i]]

    out = map2(summary_of_raster, classnames, function(x, y) ggplot(x, aes(date, median)) +
        # geom_smooth(method = "lm", color = "grey60", se = F) +
        geom_line(aes(x = date, y = med_smooth), size = 0.2) +
        geom_ribbon(aes(x = date, ymin = losd_smooth, ymax = upsd_smooth), alpha = 0.2, fill = "blue") +
        ggtitle(y) +
        theme_minimal())
    arr[[i]] = out
}

# arrange plot in grobs (lattice-like grid cells)
arr = map2(arr, proper_layernames, ~ marrangeGrob(.x, nrow=3, ncol=2, top = .y))
map(arr, ~ print(.x))

# save the plots
map2(arr, proper_layernames, ~ ggsave(filename = paste0(.y, ".png"), path = plotdir, plot = .x, width = 10, height = 10))

# PLOTY COMPLEX ----------------------------------------------------------------

View(summary)

# VH vs. CO with ribbons
s.plt1 = function(vh, co, classnames){

    # calculate range min-max
    max = map_dbl(vh, ~ max(.x$median)) %>% max()
    min = map_dbl(vh, ~ min(.x$median)) %>% min()

    pmap(list(vh, co, classnames), function(vh, co, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%
            add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                        color = I(blue_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = co, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y2", name = "S-1 VV coherence standard deviation range (1 sigma)",
                        color = I(grey_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(min, max))),
                   yaxis2 = y.co_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots1 = s.plt1(summary[["vh"]], summary[["co"]], classnames)
plots1[[1]]

walk2(plots1, classnames, ~ plotly::orca(.x, file = paste0(plotdir, "Comparing/", .y, "_bscVH-cohVV.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH vs. NDVI with ribbons
s.plt2 = function(vh, ndvi, classnames){

    # calculate range min-max
    max = map_dbl(vh, ~ max(.x$median)) %>% max()
    min = map_dbl(vh, ~ min(.x$median)) %>% min()

    ndvi_max = map_dbl(ndvi, ~ max(.x$median)) %>% max()
    ndvi_min = map_dbl(ndvi, ~ min(.x$median)) %>% min()


    pmap(list(vh, ndvi, classnames), function(vh, ndvi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%

            add_lines(data = ndvi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y3", name = "NDVI", line = ndvi.fmt,
                      connectgaps = F) %>%

            add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                        color = I(blue_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = ndvi, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y2", name = "NDVI standard deviation range (1 sigma)",
                        color = I(red_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(min, max))),
                   yaxis2 = y.s2_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots2 = s.plt2(summary[["vh"]], summary[["ndvi"]], classnames)
plots2[[1]]

walk2(plots2, classnames, ~ plotly::orca(.x, file = paste0(plotdir, "Comparing/", .y, "_bscVH-NDVI.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH, CO vs. NDVI with ribbons
s.plt3 = function(vh, co, ndvi, classnames){

    # calculate range min-max
    max = map_dbl(vh, ~ max(.x$median)) %>% max()
    min = map_dbl(vh, ~ min(.x$median)) %>% min()

    ndvi_max = map_dbl(ndvi, ~ max(.x$median)) %>% max()
    ndvi_min = map_dbl(ndvi, ~ min(.x$median)) %>% min()


    pmap(list(vh, co, ndvi, classnames), function(vh, co, ndvi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ median,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%
            add_lines(data = ndvi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y3", name = "NDVI", line = ndvi.fmt,
                      connectgaps = F) %>%
            add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                        color = I(blue_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = ndvi, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y2", name = "NDVI standard deviation range (1 sigma)",
                        color = I(red_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(min, max))),
                   yaxis2 = y.co_2,
                   yaxis3 = y.s2_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots3 = s.plt3(summary[["vh"]], summary[["co"]], summary[["ndvi"]], classnames)
plots3[[1]]

walk2(plots3, classnames, ~ plotly::orca(.x, file = paste0(plotdir, "Comparing/", .y, "_bscVH-cohVV-NDVI.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH in comparison with S2 indizes
s.plt4 = function(vh, indizes, classnames){

    # INFO:
    # indizes are a list of index data.frames

    # save indizes' name
    indizesnames = names(indizes)
    print(indizesnames)

    # concat axis literates
    uppercase = casefold(indizesnames, upper = T)
    index_axis = paste("S-2", uppercase, "index")


    # iterate over indizes
    pmap(list(indizes, index_axis, ranges), function(index, i_names, ranges){

        print(i_names)

        # iterate over classes
        pmap(list(vh, index, classnames), function(vh, index, classnames){

            print(classnames)

            plot_ly() %>%
                add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                          yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
                add_lines(data = index, x = ~ date, y = ~ med_smooth,
                          yaxis = "y2", name = paste(i_names, "smoothed"), line = ndvi.fmt,
                          connectgaps = F) %>%
                add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                            yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                            color = I(blue_background), line = list(width = 1), opacity = 0.3,
                            showlegend = T) %>%
                add_ribbons(data = index, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                            yaxis = "y2", name = paste(i_names, "standard deviation range (1 sigma)"),
                            color = I(red_background), line = list(width = 1), opacity = 0.3,
                            showlegend = T) %>%
                layout(xaxis = date.axis,
                       yaxis = c(y.s1, range = ranges.all$vh),
                       yaxis2 = c(y.indizes, range = ranges, title = i_names),
                       legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                       margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                       title = classnames)
          })


    })

}



# GO
plots4 = s.plt4(summary[["vh"]], indizes = indizes, classnames)
# map(plots4[[1]], ~ print(.x))
plots4[[1]][[2]]

# creating subplots not easy here . . .

# save iteratively
map2(plots4, i_name, function(plots, i_name){
    print(i_name)
    pwalk(list(plots, classnames, i_name), function(x, y, z){
        plotly::orca(x, file = paste0(plotdir, "Indizes/", i_name, "-bscVH_", y, ".png"), scale = 3)
    })
})

################################################################################
# Subplots for each sensor -----------------------------------------------------
# similar to ggplots in the beginning of the script

# NOT GOOD PLOT!! BUGGY!!

s.plt5 = function(data){

    plots5 = pmap(list(data, proper_layernames, ranges.all), function(data, layr_names, range){



        plt.classwise = pmap(list(data, classes.fmt.slim, classes.fmt, classnames), function(x, classes.fmt.slim, classes.fmt, classnames){

            plot_ly() %>%
                add_lines(data = x, x ~ date, y ~ med_smooth, name = classnames, line = classes.fmt, showlegend = TRUE) %>%
                add_ribbons(data = x, x ~ date, ymin ~ losd_smooth, ymax ~ upsd_smooth, line = list(width = 0),
                            color = I(classes.fmt$color), opacity = 0.3, showlegend = F) %>%
                layout(title = layr_names,
                       yaxis = c(y.s1, range = range))

        })
        plt.classwise[[1]]
        # now create facet grid
        subplot(plt.classwise, nrows = length(plt.classwise), shareX = TRUE)
    })
    return(plots5)

}


plots5 = s.plt5(summary)
plots5[[1]]

# ------------------------------------------------------------------------------
# Same, but all lines integreted in one plot per sensor type

plots6 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

    yaxis = y.s1
    yaxis$title$text = proper_layernames.axis
    yaxis = c(yaxis, range = ranges.all)

        # stack lines up
    plt = plot_ly(width = 700, height = 500)
    for (i in length(classnames):1){
        plt = add_lines(plt, data = data[[i]], x ~ date, y ~ med_smooth, name = classnames[i], line = classes.fmt[[i]], showlegend = TRUE)
    }
    plt = plt %>%
        layout(title = layr_names,
               yaxis = yaxis,
               xaxis = date.axis,
               legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
               margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))


})

plots6[[5]]

# save plots
walk2(plots6, proper_layernames, ~ plotly::orca(.x, file = paste0(plotdir, "Sensors/", .y, "_smoothed.png"), scale = 3))
walk2(plots6, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, "_smoothed.html"), selfcontained = TRUE))

# ACHTUNG: LETZTERE MÜSSEN NOCH EINMAL VON HAND VERSCHOBEN WERDEN!!!!!


# ------------------------------------------------------------------------------
# Same as plot 6 but without smoothing and other line font

plots6 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

                  yaxis = y.s1
                  yaxis$title$text = proper_layernames.axis
                  yaxis = c(yaxis, range = ranges.all)

                  # stack lines up
                  plt = plot_ly(width = 700, height = 500)
                  for (i in length(classnames):1){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ median, name = classnames[i], line = classes.fmt.slim[[i]], showlegend = TRUE)
                  }
                  plt = plt %>%
                      layout(title = layr_names,
                             yaxis = yaxis,
                             xaxis = date.axis,
                             legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                             margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))


              })

plots6[[3]]

# save plots
walk2(plots6, proper_layernames, ~ plotly::orca(.x, file = paste0(plotdir, "Sensors/", .y, ".png"), scale = 3))
walk2(plots6, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, ".html"), selfcontained = TRUE))

# ------------------------------------------------------------------------------
# Same as plot6 bis with subtle smoothing line above the raw median data

plots6 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

                  yaxis = y.s1
                  yaxis$title$text = proper_layernames.axis
                  yaxis = c(yaxis, range = ranges.all)

                  # stack lines up
                  plt = plot_ly(width = 700, height = 500)
                  for (i in length(classnames):1){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ median, name = classnames[i], line = classes.fmt.slim[[i]], showlegend = TRUE)
                  }
                  for (i in length(classnames):1){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ med_smooth, name = paste(classnames[i], "smoothed"), opacity = 0.6, line = classes.fmt[[i]], showlegend = T)
                  }

                  plt = plt %>%
                      layout(title = layr_names,
                             yaxis = yaxis,
                             xaxis = date.axis,
                             legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                             margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))


              })

plots6[[3]]

# save plots
walk2(plots6, proper_layernames, ~ plotly::orca(.x, file = paste0(plotdir, "Sensors/", .y, ".png"), scale = 3))
walk2(plots6, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, ".html"), selfcontained = TRUE))

# ------------------------------------------------------------------------------
# end (not run)
