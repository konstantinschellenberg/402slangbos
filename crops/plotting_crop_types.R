#' Plotting crop types
#' init: 18.09.2020, Konstantin Schellenberg
#'
#' TODO Croptypes:
#' 1. remove duplicated by dplyr::distinct
#' 2. remove z-Dimension
#'
#'
#'
#'
#'
#'

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)

options(max.print = 100)

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

################################################################################
# LOAD DATA --------------------------------------------------------------------
################################################################################

data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplified")

# simplified turnover data (Crop, Pasture, Fallow, NA)
turnover = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplifiedTurnover")
crop = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplified")
turnover_complex = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers")
crops_complex = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")

turnover %>% group_by(cat) %>% arrange(a) %>% print(n=200)
str(turnover)

# Clean same geometry at turnover data
# TODO: Does this need to be done on all data beforehand, incl. z-Dimension deletion?

trn = distinct(turnover, .keep_all = TRUE) %>% st_zm()
crop = distinct(crops, .keep_all = TRUE) %>% st_zm()
trn.complex = distinct(turnover_complex, .keep_all = TRUE) %>% st_zm()
crp.complex = distinct(crops_complex, .keep_all = TRUE) %>% st_zm()

print(trn, n = 200)
print(crop, n = 200)
print(trn.complex, n = 200)
print(crp.complex, n = 200)

################################################################################
# ANALYSIS ---------------------------------------------------------------------
################################################################################

# 1) Size of study area --------------------------------------------------------

a.study_area = st_area(study_area) / 1000000
a.trn = st_area(trn) %>% sum / 1000000

# 2) Landwirtschaftlich genutzte Fläche: ---------------------------------------


a.trn / a.study_area # 0.2561339 ~ 25%

# 3) davon folgendermaßen genutzt (2016-2017) ----------------------------------

pie.data

pie.data = crp.complex %>%
    group_by(a) %>%
    summarise(count = n()) %>%
    mutate(area = st_area(.)) %>%
    st_set_geometry(NULL) %>%
    `colnames<-`(c("CropType", "count", "area"))

pie.data[[9, 1]] = "Unclassified"
pie.data$area = as.numeric(pie.data$area)
print(pie.data)

pie.data$CropType = with(pie.data, reorder(CropType, area))
# reorder, also possible with dplyr (?) https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html


# Compute the position of labels
pie.data <- pie.data %>%
    mutate(prop = area / sum(pie.data$area) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop) %>%
    arrange(CropType)

# 4) Turnover rates ------------------------------------------------------------
# complex
bar.data.ori = crops_complex %>%
    dplyr::select(c(a, b, c))

a = bar.data.ori %>%
    group_by(a) %>%
    summarise(count = n()) %>%
    rename(CropType = a) %>%
    mutate("2014-2015" = st_area(.))
b = bar.data.ori %>%
    group_by(b) %>%
    summarise(count = n()) %>%
    rename(CropType = b) %>%
    mutate("2015-2016" = st_area(.))
c = bar.data.ori %>%
    group_by(c) %>%
    summarise(count = n()) %>%
    rename(CropType = c) %>%
    mutate("2016-2017" = st_area(.))

year = map(list(a, b, c), ~ st_set_geometry(.x, NULL))

bar.data.reduced = reduce(year, left_join, by = "CropType")
bar.data.reduced[[9, 1]] = "Unclassified"
bar.data.reduced = bar.data.reduced %>% dplyr::select(-starts_with("count"))
bar.data_complex = pivot_longer(bar.data.reduced, -CropType, names_to = "year", values_to = "area")
bar.data_complex$area = as.numeric(bar.data_complex$area)

# simple
bar.data.ori = crop %>%
    dplyr::select(c(a, b, c))

a = bar.data.ori %>%
    group_by(a) %>%
    summarise(count = n()) %>%
    rename(CropType = a) %>%
    mutate("2014-2015" = st_area(.))
b = bar.data.ori %>%
    group_by(b) %>%
    summarise(count = n()) %>%
    rename(CropType = b) %>%
    mutate("2015-2016" = st_area(.))
c = bar.data.ori %>%
    group_by(c) %>%
    summarise(count = n()) %>%
    rename(CropType = c) %>%
    mutate("2016-2017" = st_area(.))

year = map(list(a, b, c), ~ st_set_geometry(.x, NULL))

bar.data.reduced = reduce(year, left_join, by = "CropType")
bar.data.reduced[[4, 1]] = "Unclassified"
bar.data.reduced = bar.data.reduced %>% dplyr::select(-starts_with("count"))
bar.data_simple = pivot_longer(bar.data.reduced, -CropType, names_to = "year", values_to = "area")
bar.data_simple$area = as.numeric(bar.data_simple$area)

# saving pie data
saveRDS(bar.data_complex, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_type_complex.rds")
saveRDS(bar.data_simple, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_type_simple.rds")


# . . . and many more analyses!

################################################################################
# PLOTTING ---------------------------------------------------------------------
################################################################################

# 1) PIE CHART of different crop type as of 2016-2017

# piechart
gg.pie = ggplot(pie.data, aes(x="", y=area, fill=CropType)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0, direction = -1) +
    # geom_text(aes(y = ypos, label = prop), color = "black", size=3)+
    scale_fill_brewer(palette="RdYlGn") +
    theme(legend.position="none") +
    theme_void() # remove background, grid, numeric labels
gg.pie
ggsave(filename = "CropTypes2016-17.png", plot = gg.pie, scale = 1, path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes")

# ------------------------------------------------------------------------------
# 2) Turnover rates

# Stacked + percent
gg.bar1 = ggplot(bar.data_simple, aes(fill=CropType, y=area, x=year)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_brewer(palette="RdYlGn") +
    ylab("percentage")
gg.bar1

gg.bar2 = ggplot(bar.data_complex, aes(fill=CropType, y=area, x=year)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_brewer(palette="RdYlGn") +
    ylab("percentage")
gg.bar2

ggsave(plot = gg.bar1, filename = "CropTurnover_simple.png", path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes", scale = 1)
ggsave(plot = gg.bar2, filename = "CropTurnover_complex.png", path = "D:/Geodaten/#Jupiter/GEO402/06_plots/roptypes", scale = 1)

# ------------------------------------------------------------------------------
