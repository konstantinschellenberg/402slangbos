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

source("D:/Projects/402slangbos/import_samples.R")

commandArgs = function(...) 2
source("D:/Projects/402slangbos/import_extractedInformation.R")
source("D:/Projects/402slangbos/plotting/fonts.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/GEO402"
setwd(env)

# destination
plotdir = "06_plots/Classification/Plots"
comp = "06_plots/Classification/Plots/Summary Statistics/Comparing/"
indi = "06_plots/Classification/Plots/Summary Statistics/Indizes/"
sens = "06_plots/Classification/Plots/Summary Statistics/Sensors/"
dstdir = "03_develop/extract"

# make dirs
map(c(plotdir, comp, indi, sens, dstdir), ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))


# READ IN ----------------------------------------------------------------------

# clean data from NA (possible step in between)
# selective cleaning of NA: map_at

# not to be cleaned:
vars = c()

summary = summary %>% map_at(., .at = vars(!vars), ~ map(.x, function(x){
    na.omit(x)
}))

# INDIZES DEFINITION -----------------------------------------------------------

# give named list of indizes
indizes = list(evi = summary[["evi"]], ndvi = summary[["ndvi"]], reip = summary[["reip"]],
               msavi = summary[["msavi"]], savi = summary[["savi"]],
               dvi = summary[["dvi"]], rvi = summary[["rvi"]])

indizes = indizes[lapply(indizes, length) > 0]

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

ggplot(p.vh[[2]], aes(x = date)) +
    geom_line(aes(y = median)) +
    geom_line(aes(y = med_smooth)) +
    geom_ribbon(aes(ymin = losd_smooth, ymax = upsd_smooth), fill = "grey", alpha = 0.5) +
    theme_minimal()

arr = list()
for (i in seq_along(layernames)){
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
# map(arr, ~ print(.x))

# save the plots
# map2(arr, proper_layernames, ~ ggsave(filename = paste0(.y, ".png"), path = plotdir, plot = .x, width = 10, height = 10))

# PLOTY COMPLEX ----------------------------------------------------------------

# View(summary)

# VH vs. CO with ribbons
s.plt1 = function(vh, co, classnames){

    # calculate range min-max

    min = ranges.all$vh[[1]][1]
    max = ranges.all$vh[[1]][2]

        pmap(list(vh, co, classnames), function(vh, co, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%
            add_lines(data = vh, x = ~ date, y = ~ median,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt.slim,
                      showlegend = F) %>%
            add_lines(data = co, x = ~ date, y = ~ median,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt.slim,
                      connectgaps = F, showlegend = F) %>%
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

walk2(plots1, classnames, ~ plotly::orca(.x, file = paste0(comp, .y, "_bscVH-cohVV.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH vs. NDVI with ribbons
s.plt2 = function(vh, ndvi, classnames){

    pmap(list(vh, ndvi, classnames), function(vh, ndvi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%

            add_lines(data = ndvi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "NDVI", line = ndvi.fmt.slim,
                      connectgaps = F) %>%
            add_lines(data = ndvi, x = ~ date, y = ~ median,
                      yaxis = "y2", name = "NDVI", line = ndvi.fmt.slim,
                      connectgaps = F) %>%
            add_lines(data = vh, x = ~ date, y = ~ median,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt.slim) %>%
            add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                        color = I(blue_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = ndvi, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y2", name = "NDVI standard deviation range (1 sigma)",
                        color = I(red_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(ranges.all$vh[[1]][1], ranges.all$vh[[1]][2]))),
                   yaxis2 = y.s2_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots2 = s.plt2(summary[["vh"]], summary[["ndvi"]], classnames)
plots2[[3]]

walk2(plots2, classnames, ~ plotly::orca(.x, file = paste0(comp, .y, "_bscVH-NDVI.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH, CO vs. NDVI with ribbons
s.plt3 = function(vh, co, ndvi, classnames){

    pmap(list(vh, co, ndvi, classnames), function(vh, co, ndvi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%
            add_lines(data = ndvi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y3", name = "NDVI", line = ndvi.fmt,
                      connectgaps = F) %>%
            add_ribbons(data = vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                        color = I(blue_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = co, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y2", name = "S-1 VV Coherence standard deviation range (1 sigma)",
                        color = I(red_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            add_ribbons(data = ndvi, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                        yaxis = "y3", name = "NDVI standard deviation range (1 sigma)",
                        color = I(red_background), line = list(width = 1), opacity = 0.3,
                        showlegend = T) %>%
            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(ranges.all$vh[[1]][1], ranges.all$vh[[1]][2]))),
                   yaxis2 = y.co_2,
                   yaxis3 = y.s2_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots3 = s.plt3(summary[["vh"]], summary[["co"]], summary[["ndvi"]], classnames)
plots3[[1]]

walk2(plots3, classnames, ~ plotly::orca(.x, file = paste0(comp, .y, "_bscVH-cohVV-NDVI.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH in comparison with S2 indizes
s.plt4 = function(vh, indizes, classnames){

    # INFO:
    # indizes are a list of index data.frames

    # save indizes' name
    indizesnames = names(indizes)
    cat(indizesnames)

    # concat axis literates
    uppercase = casefold(indizesnames, upper = T)
    index_axis = paste("S-2", uppercase, "index")


    # iterate over indizes
    pmap(list(indizes, index_axis, ranges), function(index, i_names, ranges){

        print(i_names)

        # iterate over classes
        pmap(list(vh, index, classnames), function(vh, index, classnames){

            print(classnames)

            plot_ly(width = 700, height = 500) %>%
                add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                          yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
                add_lines(data = index, x = ~ date, y = ~ med_smooth,
                          yaxis = "y2", name = paste(i_names, "smoothed"), line = ndvi.fmt,
                          connectgaps = F) %>%
                add_lines(data = index, x = ~ date, y = ~ median,
                          yaxis = "y2", name = paste(i_names, "median"), line = ndvi.fmt.slim,
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
        plotly::orca(x, file = paste0(indi, i_name, "-bscVH_", y, ".png"), scale = 3)
    })
})


# ------------------------------------------------------------------------------
# All classes in Plot per sensor type

plots6 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

    yaxis = y.s1
    yaxis$title$text = proper_layernames.axis
    yaxis = c(yaxis, range = ranges.all)

        # stack lines up
    plt = plot_ly(width = 700, height = 500)
    for (i in length(classnames):1){
        plt = add_lines(plt, data = data[[i]], x ~ date, y ~ med_smooth, name = classnames[i], line = classes.fmt.slim[[i]], showlegend = TRUE)
    }
    plt = plt %>%
        layout(title = layr_names,
               yaxis = yaxis,
               xaxis = date.axis,
               legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
               margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))


})

plots6[[1]]

# save plots
walk2(plots6, proper_layernames, ~ plotly::orca(.x, file = paste0(sens, .y, "_smoothed.png"), scale = 3))
setwd(sens)
walk2(plots6, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, "_smoothed.html"), selfcontained = TRUE))
setwd(env)
# ACHTUNG: LETZTERE MÜSSEN NOCH EINMAL VON HAND VERSCHOBEN WERDEN!!!!!

# ------------------------------------------------------------------------------
# Same as plot 6 but without smoothing and other line font

plots7 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

                  yaxis = y.s1
                  yaxis$title$text = proper_layernames.axis
                  yaxis = c(yaxis, range = ranges.all)

                  # stack lines up
                  plt = plot_ly(width = 700, height = 500)
                  for (i in 1:length(classnames)){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ median, name = classnames[i], line = classes.fmt.slim[[i]], showlegend = TRUE)
                      if (i == 4) break
                  }
                  plt = plt %>%
                      layout(title = layr_names,
                             yaxis = yaxis,
                             xaxis = date.axis,
                             legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                             margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))


              })

plots7[[3]]

# save plots
walk2(plots7, proper_layernames, ~ plotly::orca(.x, file = paste0(sens, .y, ".png"), scale = 3))
setwd(sens)
walk2(plots7, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, ".html"), selfcontained = TRUE))
setwd(env)
# ------------------------------------------------------------------------------
# Same as plot6 bis with subtle smoothing line above the raw median data

plots8 = pmap(list(summary, proper_layernames, proper_layernames.axis, ranges.all),
              function(data, layr_names, proper_layernames.axis, ranges.all){

                  yaxis = y.s1
                  yaxis$title$text = proper_layernames.axis
                  yaxis = c(yaxis, range = ranges.all)

                  # stack lines up
                  plt = plot_ly(width = 700, height = 500)
                  for (i in 1:length(classnames)){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ median, name = classnames[i], line = classes.fmt.slim[[i]], showlegend = TRUE)
                      if (i == 4) break
                  }
                  for (i in 1:length(classnames)){
                      plt = add_lines(plt, data = data[[i]], x ~ date, y ~ med_smooth, name = paste(classnames[i], "smoothed"), opacity = 0.6, line = classes.fmt[[i]], showlegend = T)
                      if (i == 4) break
                  }

                  plt = plt %>%
                      layout(title = layr_names,
                             yaxis = yaxis,
                             xaxis = date.axis,
                             legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                             margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE))
              })

plots8[[2]]

# save plots
walk2(plots8, proper_layernames, ~ plotly::orca(.x, file = paste0(sens, .y, "_with_smooth.png"), scale = 3))
setwd(sens)
walk2(plots8, proper_layernames, ~ htmlwidgets::saveWidget(widget = .x, file = paste0(.y, "_with_smooth.html"), selfcontained = TRUE))
setwd(env)

# ------------------------------------------------------------------------------
# Classwise plots: VH, COH, NDVI, SAVI

plots9 = function(vh, co, ndvi, savi, classnames){

    size_markers = 3

    pmap(list(vh, co, ndvi, savi, classnames), function(vh, co, ndvi, savi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%

            # Sentinel-2 markers
            add_markers(data = ndvi, x = ~ date, y = ~ median,
                        yaxis = "y3", name = "NDVI",
                        opacity = 0.8,
                        marker = list(
                            color = "#017a32",
                            size = size_markers,
                            line = list(
                                color = "grey80",
                                width = 0.2
                            ))) %>%
            add_markers(data = savi, x = ~ date, y = ~ median,
                        yaxis = "y3", name = "SAVI",
                        opacity = 0.8,
                        marker = list(
                            color = "black",
                            size = size_markers,
                            line = list(
                                color = "grey80",
                                width = 0.2
                            ))) %>%

            # smoothed lines indizes
            add_lines(data = ndvi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y3", name = "S-2 NDVI",
                      opacity = 0.7,
                      line = ndvi.green.fmt) %>%
            add_lines(data = savi, x = ~ date, y = ~ med_smooth,
                      yaxis = "y3", name = "S-2 SAVI",
                      opacity = 0.7,
                      line = savi.fmt) %>%


            # smoothed lines
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%

            # median lines
            add_lines(data = vh, x = ~ date, y = ~ median,
                      yaxis = "y1", name = "S-1 VH backscatter", line = vh.fmt.slim) %>%
            add_lines(data = co, x = ~ date, y = ~ median,
                      yaxis = "y2", name = "S-1 VV coherence (2-weeks interval)", line = red.fmt.slim,
                      connectgaps = F) %>%

            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(ranges.all$vh[[1]][1], ranges.all$vh[[1]][2]))),
                   yaxis2 = y.co_2,
                   yaxis3 = y.ndvi_savi,
                   yaxis4 = y.savi,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 120, automargin = TRUE),
                   title = classnames)
    })

}

plts9 = plots9(summary[["vh"]], summary[["co"]], summary[["ndvi"]], summary[["savi"]], classnames)

plts9[[1]]
plts9[[2]]
plts9[[3]]
plts9[[4]]
plts9[[5]]

walk2(plts9, classnames, ~ plotly::orca(.x, file = paste0(comp, .y, "_bscVH-cohVV-NDVI-SAVI.png"), scale = 3))

# ------------------------------------------------------------------------------
# Classwise plots: VH, COH, NDVI, SAVI

plots10 = function(vh, co, ndvi, savi, classnames){

    size_markers = 3

    pmap(list(vh, co, ndvi, savi, classnames), function(vh, co, ndvi, savi, classnames){

        plt = plot_ly(width = 700, height = 500) %>%

            # Sentinel-2 markers
            add_markers(data = ndvi, x = ~ date, y = ~ median,
                        yaxis = "y3", name = "NDVI",
                        opacity = 0.8,
                        marker = list(
                            color = "#017a32",
                            size = size_markers,
                            line = list(
                                color = "grey80",
                                width = 0.2
                            ))) %>%
            add_markers(data = savi, x = ~ date, y = ~ median,
                        yaxis = "y3", name = "SAVI",
                        opacity = 0.8,
                        marker = list(
                            color = "black",
                            size = size_markers,
                            line = list(
                                color = "grey80",
                                width = 0.2
                            ))) %>%

            # smoothed lines
            add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                      yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
            add_lines(data = co, x = ~ date, y = ~ med_smooth,
                      yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                      connectgaps = F) %>%

            # median lines
            add_lines(data = vh, x = ~ date, y = ~ median,
                      yaxis = "y1", name = "S-1 VH backscatter", line = vh.fmt.slim) %>%
            add_lines(data = co, x = ~ date, y = ~ median,
                      yaxis = "y2", name = "S-1 VV coherence (2-weeks interval)", line = red.fmt.slim,
                      connectgaps = F) %>%

            layout(xaxis = date.axis,
                   yaxis = c(y.s1, range = list(c(-24, -14))),
                   yaxis2 = y.co_2,
                   yaxis3 = y.ndvi_savi,
                   yaxis4 = y.savi,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 120, automargin = TRUE),
                   title = classnames)
    })

}

plts10 = plots10(summary[["vh"]], summary[["co"]], summary[["ndvi"]], summary[["savi"]], classnames)
plts10[[1]]
plts10[[2]]
plts10[[3]]
plts10[[4]]
plts10[[7]]

walk2(plts10, classnames, ~ plotly::orca(.x, file = paste0(comp, .y, "_bscVH-cohVV-NDVI-SAVI.png"), scale = 3))

# ------------------------------------------------------------------------------
# fuse increase and continuous!

plots11 = function(in.vh, in.co, in.ndvi, in.savi){

    rasters = list(in.vh, in.co, in.ndvi, in.savi)
    in.names = c("vh", "co", "ndvi", "savi")

    # fiter these classes
    # Increase + Continuous

    classes = c(1, 2)

    # define averaging function
    fuse_data = function(ras, classes){
        r1 = ras[[classes[1]]]
        r2 = ras[[classes[2]]]

        # get dates col
        date = r1$date

        r1 = r1 %>% dplyr::select(-date)
        r2 = r2 %>% dplyr::select(-date)

        # averaging
        r = map2_df(r1, r2, function(x, y){
            (x + y) / 2
        })
        r = r %>% mutate(date = date)

    }

    # action average between classes
    data = map(rasters, function(x){
        fuse_data(x, classes)
    })

    names(data) = in.names

    # redefine layers
    vh = data[["vh"]]
    co = data[["co"]]
    ndvi = data[["ndvi"]]
    savi = data[["savi"]]


    size_markers = 3

    # calculate range min-max
    vh_max = max(vh$median)
    vh_min = min(vh$median)

    ndvi_max = max(ndvi$median)
    ndvi_min = min(ndvi$median)

    savi_max = max(savi$median)
    savi_min = min(savi$median)

    co_max = max(co$median)
    co_min = min(co$median)


    print(vh_max);print(vh_min)
    print(co_max);print(co_min)

        plt = plot_ly(width = 700, height = 500) %>%

        # Sentinel-2 markers
        add_markers(data = ndvi, x = ~ date, y = ~ median,
                    yaxis = "y3", name = "NDVI",
                    opacity = 0.8,
                    marker = list(
                        color = "#017a32",
                        size = size_markers,
                        line = list(
                            color = "grey80",
                            width = 0.2
                        ))) %>%
        add_markers(data = savi, x = ~ date, y = ~ median,
                    yaxis = "y3", name = "SAVI",
                    opacity = 0.8,
                    marker = list(
                        color = "black",
                        size = size_markers,
                        line = list(
                            color = "grey80",
                            width = 0.2
                            ))) %>%

        # smoothed lines
        add_lines(data = vh, x = ~ date, y = ~ med_smooth,
                  yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
        add_lines(data = co, x = ~ date, y = ~ med_smooth,
                  yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                  connectgaps = F) %>%

        # median lines
        add_lines(data = vh, x = ~ date, y = ~ median,
                  yaxis = "y1", name = "S-1 VH backscatter", line = vh.fmt.slim) %>%
        add_lines(data = co, x = ~ date, y = ~ median,
                  yaxis = "y2", name = "S-1 VV coherence (2-weeks interval)", line = red.fmt.slim,
                  connectgaps = F) %>%

        layout(xaxis = date.axis,
               yaxis = c(y.s1, range = list(c(-24, -14))),
               yaxis2 = y.co_2,
               yaxis3 = y.ndvi_savi,
               yaxis4 = y.savi,
               legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
               margin = list(pad = 0, b = 200, l = 0, r = 120, automargin = TRUE),
               title = classnames)

}

plts11 = plots11(summary_all[["vh"]], summary[["co"]], summary[["ndvi"]], summary[["savi"]])
plts11

plotly::orca(plts11, file = paste0(plotdir, "Summary Statistics/Comparing/", "INC-CON_bscVH-cohVV-NDVI-SAVI.png"), scale = 3)

# end (not run)
