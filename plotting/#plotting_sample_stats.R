#' Plotting Sample Statistics revisited
#' init: 24.09.2020

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

library(exactextractr)

options(max.print=100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/fonts.R")
source("D:/Geodaten/Master/projects/402slangbos/plotting/Mapviews.R")

# RASTERS ----------------------------------------------------------------------

rasters = list(co, dvi, evi, msavi, ndvi, reip, rvi, vh, vv)
layernames = c("co", "dvi", "evi", "msavi", "ndvi", "reip", "rvi", "vh", "vv")
# proper_layernames = c("Sentinel-1 VH", "Sentinel-1 VV", "Sentinel-1 VV Coherence", "Sentinel-2 DVI", "Sentinel-2 EVI",
#                       "Sentinel-2 MSAVI", "Sentinel-2 NDVI", "Sentinel-2 REIP", "Sentinel-2 RVI")
# proper_layernames.axis = c("S-1 VH [dB]", "S-1 VV [dB]", "S-1 VV Coherence", "S-2 DVI Index", "S-2 EVI Index",
#                            "S-2 MSAVI Index", "S-2 NDVI Index", "S-2 REIP Index", "S-2 RVI Index")
names(rasters) = layernames
classnames = c("Slangbos Increase", "Slangbos Continuous", "Slangbos Clearning", "Grassland", "Cultivated")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
plotdir = "06_plots/"
destdir = "03_develop/extract/"

# READ IN MASTER DATA ----------------------------------------------------------

master = readRDS("03_develop/extract/extract_all.RDS")
extract

# EXAMPLE SITES ------------------------------------------------------------

samples = list(c(1, 11), c(1, 15), c(1, 26), c(1, 44), c(1, 45), # Increase
     c(2, 8), c(2, 17), c(2, 22), c(2, 31), c(2, 32), # Continuous
     c(3, 2), c(3, 3), c(3, 4), c(3, 14), c(3, 16), # Clearing
     c(4, 2), c(4, 4), c(4, 7), c(4, 13), # Grassland
     c(5, 1), c(5, 3), c(5, 4), c(5, 12)) # Agriculture

# show sample site on map
map(rev(samples.comb), function(x){
    class = x[1]
    sample = x[2]
    mapping(class, sample)
})


ex_position = gt %>% position(1, 15)

View(master)

ex = master %>% filter(class == "Slangbos Increase" & sample == 15)
ex = ex %>% filter(sensor == "vh" | sensor == "ndvi" | sensor == "co")
ex = ex %>% na.omit()


m.vh = filter(ex, sensor == "vh")
m.co = filter(ex, sensor == "co")
m.ndvi = filter(ex, sensor == "ndvi")
# lst = list(m.vh, m.co, m.ndvi)

ggplot() +
    geom_point(aes(m.vh$date, m.vh$med))

# plotting all graphs of all classes for VH ------------------------------------

pmap(list(extract, layernames, proper_layernames.axis, proper_layernames),
     function(extr, lnames, plnames.axis, plnames){
    sensor_used = lnames
    sensor = extr

    helper.table = master %>% filter(sensor == sensor_used)
    s.min = min(helper.table$med_smooth, na.rm = TRUE)
    s.max = max(helper.table$med_smooth, na.rm = TRUE)


    map2(sensor, classnames, function(classes, name){

        # map inputs
        sample.nr = 1:length(classes)
        cat(sample.nr)


        plts = map2(classes, sample.nr, function(sample, sn){

            # transformation coefficient
            s = na.omit(sample)
            # gg = rbindlist(input, idcol = "sensors", use.names = TRUE) %>%
            #     na.omit()

            # PLOTLY TRY
            # plot_ly() %>%
            #     add_lines(data = g1, x = ~ date, y = ~ g1$med, yaxis = "y1", line = vh.fmt) %>%
            #     # add_lines(data = g2, x = ~ date, y = ~ g2$med, yaxis = "y2", line = red.fmt) %>%
            #     layout(xaxis = date.axis,
            #            yaxis = c(y.s1, range = list(c(vh_min, vh_max))),
            #            yaxis2 = y.s2_2,
            #            showlegend = F,
            #            margin = list(pad = 0, b = 50, l = 0, r = 50, automargin = TRUE),
            #            title = paste("No.", "<b>", sn))

            # GGPLOT TRY
            ggplot() +
                geom_line(aes(s$date, s$med), color = "darkgrey") +
                geom_line(aes(s$date, s$med_smooth), color = "black") +
                # geom_line(aes(g2$date, g2$med), color = "darkgreen") +
                # scale_y_continuous(name = "first", sec.axis = sec_axis(trans = ~ . -50, name = "ndvi")) +
                theme_minimal() +
                xlab("date")+
                ylab(plnames.axis)+
                ylim(c(s.min, s.max)) +
                ggtitle(paste("No.:", sn))
        })

        p1 = plts[[1]]; p2 = plts[[2]]
        p1

        # plotly
        # subplot(plts, shareX = TRUE, nrows = 5)

        g1 = marrangeGrob(plts, nrow = 5, ncol = 10, top = paste(plnames, name))
        ggsave(paste0(sensor_used, "_", name, ".png"), plot = g1, path = paste0(plotdir, "Samples/Facets/"), device = "png",
               scale = 1)

    })

})

plt.bolt = plot_ly(width = 700, height = 500) %>%
    add_lines(data = m.vh, x = ~ date, y = ~ med_smooth,
              yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.big.fmt) %>%
    add_lines(data = m.co, x = ~ date, y = ~ med_smooth,
              yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks baseline)", line = red.big.fmt,
              connectgaps = F) %>%
    # add_lines(data = m.ndvi, x = ~ date, y = ~ med_smooth,
    #           yaxis = "y3", name = "NDVI", line = ndvi.fmt,
    #           connectgaps = F) %>%
    # add_ribbons(data = m.vh, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
    #             yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
    #             color = I(blue_background), line = list(width = 1), opacity = 0.3,
    #             showlegend = T) %>%
    # add_ribbons(data = m.co, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
    #             yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks baseline)",
    #             color = I(orange_background), line = list(width = 1), opacity = 0.3,
    #             showlegend = T) %>%
    # add_ribbons(data = m.ndvi, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
    #             yaxis = "y3", name = "NDVI standard deviation range (1 sigma)",
    #             color = I(red_background), line = list(width = 1), opacity = 0.3,
    #             showlegend = T) %>%
    layout(xaxis = date.axis.big,
           yaxis = c(y.s1.big, range = list(c(min, max))),
           yaxis2 = y.co_2.big,
           # yaxis3 = y.s2_2,
           legend = list(font = f5, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
           showlegend=F,
           margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
           title = "")
plt.bolt


mapview(ex)

# an einzelnes Sample anpassen!
# VH, CO vs. NDVI with ribbons
s.plt1 = function(vh, co, ndvi, classnames){

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
