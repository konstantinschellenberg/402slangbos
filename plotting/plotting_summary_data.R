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

options(max.print=999999)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(vh, vv, co, dvi, evi, msavi, ndvi, reip, rvi)
layernames = c("vh", "vv", "co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi")
proper_layernames = c("S-1 VH", "S-1 VV", "S-1 VV Coherence", "S-2 DVI", "S-2 EVI",
                       "S-2 MSAVI", "S-2 NDVI", "S-2 REIP", "S-2 RVI")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"
plotdir = "06_plots/"
summarystats_plotsdir = "06_plots/summary/"

# READ IN ----------------------------------------------------------------------
data = readRDS(paste0(dstdir, "summary_statistics.RDS"))

# clean data from NA (possible step in between)
summary = map(data, ~ map(.x, function(x){
    na.omit(x)
})
)


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
            layout(xaxis = x,
                   yaxis = c(y.s1, range = list(c(min, max))),
                   yaxis2 = y.co_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots1 = s.plt1(summary[["vh"]], summary[["co"]], classnames)
plots1[[1]]

walk2(plots1, classnames, ~ plotly::orca(.x, file = paste0(summarystats_plotsdir, .y, "_bscVH-cohVV.png"), scale = 3))

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
            layout(xaxis = x,
                   yaxis = c(y.s1, range = list(c(min, max))),
                   yaxis2 = y.s2_2,
                   legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                   margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                   title = classnames)
    })

}

plots2 = s.plt2(summary[["vh"]], summary[["ndvi"]], classnames)
plots2[[1]]

walk2(plots2, classnames, ~ plotly::orca(.x, file = paste0(summarystats_plotsdir, .y, "_bscVH-NDVI.png"), scale = 3))

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
            layout(xaxis = x,
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

walk2(plots3, classnames, ~ plotly::orca(.x, file = paste0(summarystats_plotsdir, .y, "_bscVH-cohVV-NDVI.png"), scale = 3))

# ------------------------------------------------------------------------------

# VH in comparison with S2 indizes
s.plt4 = function(vh, indizes, classnames){

    # INFO:
    # indizes are a list of index data.frames

    ranges = map(indizes, function(x){
        max = map_dbl(x, ~ max(.x$median)) %>% max()
        min = map_dbl(x, ~ min(.x$median)) %>% min()
        range = list(c(min, max))
    })

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

            plot_ly(width = 700, height = 500) %>%
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
                layout(xaxis = x,
                       yaxis = c(y.s1, range = list(c(min, max))),
                       yaxis2 = c(y.indizes, range = range, title = i_names),
                       legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
                       margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
                       title = classnames)
          })


    })

}

# give named list of indizes
indizes = list(evi = summary[["evi"]], ndvi = summary[["ndvi"]], reip = summary[["reip"]],
               msavi = summary[["msavi"]], dvi = summary[["dvi"]], rvi = summary[["rvi"]])
i_name = names(indizes)

plots4 = s.plt4(summary[["vh"]], indizes = indizes, classnames)
plots4[[1]][[1]]

# save iteratively
map2(plots4, i_name, function(plots, i_name){
    print(i_name)
    pwalk(list(plots, classnames, i_name), function(x, y, z){
        plotly::orca(x, file = paste0(summarystats_plotsdir, "indizes/", y, "_bscVH-", i_name, ".png"), scale = 3)
    })
})
# ------------------------------------------------------------------------------

