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

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
plotdir = "06_plots/"
destdir = "03_develop/extract/"

# READ IN MASTER DATA ----------------------------------------------------------

# TIDY DATA.FRAME
master = readRDS("03_develop/extract/extract_all.RDS")

# NAMED LISTS
# View(extract)

# EXAMPLE SITES ------------------------------------------------------------

samples.comb = list(c(1, 11), c(1, 15), c(1, 25), c(1, 42), c(1, 43), # Increase
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

# SITES TO BE DELETED ----------------------------------------------------------
# because they are not showing proper Increase patterns

# 1-17
# 1-27
# 1-46
# 1-47

################################################################################
# plotting all graphs of all classes for VH ------------------------------------
################################################################################

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
               scale = 2)

    })

})

# GGPLOT EXAMPLE ---------------------------------------------------------------

ex = master %>% filter(class == "Slangbos Increase" & sample == 15)
ex = ex %>% filter(sensor == "vh" | sensor == "ndvi" | sensor == "co")
ex = ex %>% na.omit()


m.vh = filter(ex, sensor == "vh")
m.co = filter(ex, sensor == "co")
m.ndvi = filter(ex, sensor == "ndvi")
# lst = list(m.vh, m.co, m.ndvi)

ggplot() +
    geom_point(aes(m.vh$date, m.vh$med))

plt.bolt = plot_ly(width = 700, height = 500) %>%
    add_lines(data = m.vh, x = ~ date, y = ~ med_smooth,
              yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.big.fmt) %>%
    add_lines(data = m.co, x = ~ date, y = ~ med_smooth,
              yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks baseline)", line = red.big.fmt,
              connectgaps = F) %>%
    layout(xaxis = date.axis.big,
           yaxis = c(y.s1.big, range = list(c(min, max))),
           yaxis2 = y.co_2.big,
           # yaxis3 = y.s2_2,
           legend = list(font = f5, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
           showlegend=F,
           margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
           title = "")
plt.bolt

# Stats for each Sample Site ---------------------------------------------------
# BAUSTELLE

# an einzelnes Sample anpassen!
# VH, CO vs. NDVI with ribbons

plt.samples = map(samples.comb, function(x){

    # grep combinations
    class = x[1]
    sample = x[2]

    vh_data = extract[["vh"]][[class]][[sample]] %>% na.omit()
    co_data = extract[["co"]][[class]][[sample]] %>% na.omit()
    ndvi_data = extract[["ndvi"]][[class]][[sample]] %>% na.omit()
    dim(vh_data)



     plt = plot_ly(width = 700, height = 500) %>%
        add_lines(data = vh_data, x = ~ date, y = ~ med_smooth,
                  yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt) %>%
         add_lines(data = vh_data, x = ~ date, y = ~ med,
                   yaxis = "y1", name = "S-1 VH backscatter smoothed", line = vh.fmt.slim) %>%
        add_lines(data = co_data, x = ~ date, y = ~ med,
                  yaxis = "y2", name = "S-1 VV coherence smoothed (2-weeks interval)", line = red.fmt,
                  connectgaps = F) %>%
        add_lines(data = ndvi_data, x = ~ date, y = ~ med_smooth,
                  yaxis = "y3", name = "NDVI", line = ndvi.fmt,
                  connectgaps = T) %>%
        add_ribbons(data = vh_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                    yaxis = "y1", name = "S-1 VH standard deviation range (1 sigma)",
                    color = I(blue_background), line = list(width = 1), opacity = 0.3,
                    showlegend = T) %>%
        add_ribbons(data = ndvi_data, x = ~date, ymin = ~losd_smooth, ymax = ~upsd_smooth,
                    yaxis = "y2", name = "NDVI standard deviation range (1 sigma)",
                    color = I(red_background), line = list(width = 1), opacity = 0.3,
                    showlegend = T) %>%
        layout(xaxis = date.axis,
               yaxis = c(y.s1, range = list(c(min, max))),
               yaxis2 = y.co_2,
               yaxis3 = y.s2_2,
               legend = list(font = f1, orientation = "h", xanchor = "center", yanchor = "bottom", y = -0.7, x = 0.5),
               margin = list(pad = 0, b = 200, l = 0, r = 100, automargin = TRUE),
               title = paste(classnames[class], sample))
     plotly::orca(plt, file = paste0(plotdir, "Samples/", paste(classnames[class], sample, sep = "_"), ".png"), scale = 3)

     })
