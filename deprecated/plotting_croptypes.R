# Plotting stats for Croptypes

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)

library(grid)
library(gridExtra)

options(max.print = 100)

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# PLOTTING ---------------------------------------------------------------------

data3 = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers")
sfc = st_geometry(data3)
data3 = st_set_geometry(data3, NULL) %>%
    as_tibble()

g1 = ggplot(data3) +
    geom_bar(aes(turnover)) +
    theme_bw()

# ggsave(plot = g1, filename = "turnover_amount.png", path = "06_plots/croptypes", scale = 1.5)

colfunc = colorRampPalette(c("white", "darkred"))

g2 = ggplot(data3) +
    geom_boxplot(aes(a, turnover), fill = "lightgrey", varwidth = T) +
    theme_bw()

p1 = data3 %>%
    group_by(a) %>%
    summarise(mean.trn = mean(turnover),
              sd = sd(turnover),
              var = sd(turnover)^2)

g3 = ggplot(p1) +
    geom_point(aes(a, mean.trn, size = var), shape = 16) +
    ylab("Mean Field Turnover") +
    xlab("First Year Sowing") +
    theme_bw()

facet2 = grid.arrange(g2, g3, nrow = 2)
# ggsave(plot = facet2, filename = "turnover.png", path = "06_plots/croptypes")
