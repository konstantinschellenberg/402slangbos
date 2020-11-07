# Testing supsmu-parameters

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)

library(exactextractr)

options(max.print = 200)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# destination
dstdir = "03_develop/extract/"

# DATA -------------------------------------------------------------------------
mdf = summary$savi$`1` %>% na.omit()

# METRIC ANALYSIS --------------------------------------------------------------
# BASS -------------------------------------------------------------------------

bass_settings = seq(0, 10, 1)

# tests
tests = map(bass_settings, ~ supsmu(mdf$date, mdf$median, bass = .x))
map(tests, function(x) plot(x$x, x$y, add = TRUE))

gg = NULL
gg = ggplot() +
    geom_point(data = mdf, aes(x = date, y = median), color = "red") +
    geom_line(data = mdf, aes(x = date, y = median), color = "red") +
    ylim(c(0, 0.4))

# purrr's map ist leider nicht möglich hier...
for (i in seq_along(tests)){
    df = data.frame(x = tests[[i]]$x, y = tests[[i]]$y)
    print(head(df))
    gg = gg + geom_point(data = df, aes(x, y), size = 0.6)
}
gg = gg + ggtitle("SAVI Slangbos Increase Supersmoother `bass` metric [0-10]",
             subtitle = "0 was used so far (least smoothed)")
gg


# SPAN -------------------------------------------------------------------------

# span settings, default: "cv" derived
span_settings = seq(0.2, 0.4, 0.02)

tests = map(bass_settings, ~ supsmu(mdf$date, mdf$median, bass = 0, span = span_settings))
map(tests, function(x) plot(x$x, x$y))

gg = NULL
gg = ggplot() +
    geom_point(data = mdf, aes(x = date, y = median), color = "red") +
    geom_line(data = mdf, aes(x = date, y = median), color = "red") +
    ylim(c(0, 0.4))

# purrr's map ist leider nicht möglich hier...
for (i in seq_along(tests)){
    df = data.frame(x = tests[[i]]$x, y = tests[[i]]$y)
    print(head(df))
    gg = gg + geom_point(data = df, aes(x, y), size = 0.6)
}
gg = gg + ggtitle("SAVI Slangbos Increase Supersmoother `span` metric [0-10]",
                  subtitle = "0 was used so far (least smoothed)")
gg
