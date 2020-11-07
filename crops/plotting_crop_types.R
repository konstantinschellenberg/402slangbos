#' Plotting crop types
#' init: 18.09.2020, Konstantin Schellenberg
#'
#' TODO Croptypes:
#' 1. remove duplicated by dplyr::distinct
#' 2. remove z-Dimension

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)
library(scales)

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
crops = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplified")
turnover_complex = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers")
crops_complex = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")

turnover %>% group_by(cat) %>% arrange(a) %>% print(n=200)
str(turnover)

# Clean same geometry at turnover data
# TODO: Does this need to be done on all data beforehand, incl. z-Dimension deletion?

trn = distinct(turnover, .keep_all = TRUE)
crp = distinct(crops, .keep_all = TRUE)
trn.complex = distinct(turnover_complex, .keep_all = TRUE)
crp.complex = distinct(crops_complex, .keep_all = TRUE)

print(trn, n = 200)
print(crp, n = 200)
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

# 3) davon folgendermaßen genutzt (2015-2016) ----------------------------------

pie.data = crp.complex %>%
    group_by(b) %>%
    summarise(count = n()) %>%
    mutate(area = st_area(.)) %>%
    st_set_geometry(NULL) %>%
    `colnames<-`(c("CropType", "count", "area"))

pie.data$CropType = pie.data$CropType %>% recode_factor("X" = "Unclassified")
pie.data$area = as.numeric(pie.data$area)
print(pie.data)

# now drop unwanted land use classes
pie.data = dplyr::filter(pie.data, CropType != "Groundnuts")
pie.data %>% arrange(area)

pie.data$CropType = with(pie.data, reorder(CropType, area))
# reorder, also possible with dplyr (?) https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html

# Compute the position of labels
pie.data <- pie.data %>%
    mutate(prop = area / sum(pie.data$area) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop) %>%
    arrange(CropType)

# saving pie data
saveRDS(bar.data_complex, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_type_complex.rds")
saveRDS(bar.data_simple, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_type_simple.rds")

# 4) Turnover rates ------------------------------------------------------------
# complex
bar.data.ori = crops_complex %>%
    dplyr::select(c(a, b, c, d))

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
d = bar.data.ori %>%
    group_by(d) %>%
    summarise(count = n()) %>%
    rename(CropType = d) %>%
    mutate("2017-2018" = st_area(.))

year = map(list(a, b, c, d), ~ st_set_geometry(.x, NULL))

bar.data.reduced = reduce(year, left_join, by = "CropType")

# drop unwanted
bar.data.reduced$CropType = bar.data.reduced$CropType %>% recode_factor("X" = "Unclassified")
bar.data.reduced = dplyr::filter(bar.data.reduced, CropType != "Groundnuts")

bar.data.reduced = bar.data.reduced %>% dplyr::select(-starts_with("count"))
bar.data_complex = pivot_longer(bar.data.reduced, -CropType, names_to = "year", values_to = "area")
bar.data_complex$area = as.numeric(bar.data_complex$area)

# simple
bar.data.ori = crp %>%
    dplyr::select(c(a, b, c, d))

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
d = bar.data.ori %>%
    group_by(d) %>%
    summarise(count = n()) %>%
    rename(CropType = d) %>%
    mutate("2017-2018" = st_area(.))

year = map(list(a, b, c, d), ~ st_set_geometry(.x, NULL))

bar.data.reduced = reduce(year, left_join, by = "CropType")

# drop unwanted
bar.data.reduced$CropType = bar.data.reduced$CropType %>% recode_factor("X" = "Unclassified")
bar.data.reduced = dplyr::filter(bar.data.reduced, CropType != "Groundnuts")

bar.data.reduced = bar.data.reduced %>% dplyr::select(-starts_with("count"))
bar.data_simple = pivot_longer(bar.data.reduced, -CropType, names_to = "year", values_to = "area")
bar.data_simple$area = as.numeric(bar.data_simple$area)

# save tables for complex data
write_excel_csv(bar.data.reduced, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_types_wide.csv", append = FALSE, col_names = TRUE)
write_excel_csv(bar.data_complex, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_types_long.csv", append = FALSE, col_names = TRUE)
write_excel_csv(bar.data_simple, "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/crop_types_simple_long.csv", append = FALSE, col_names = TRUE)



# 5) INTERSECTION WITH SLANGBOS PLOTS-------------------------------------------

################################################################################
# PLOTTING ---------------------------------------------------------------------
################################################################################

# 1) PIE CHART of different crop types as of 2015-2016

# piechart
gg.pie = ggplot(pie.data, aes(x="", y=area, fill=CropType)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0, direction = -1) +
    # geom_text(aes(y = ypos, label = prop), color = "black", size=3)+
    scale_fill_brewer(palette="RdYlGn") +
    theme(legend.position="none") +
    theme_void() # remove background, grid, numeric labels
gg.pie
ggsave(filename = "CropTypes2015-2016.png", plot = gg.pie, scale = 1, path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes")

# without pasture
pie.data2 = pie.data %>% filter(CropType != "Pasture")
gg.pie2 = ggplot(pie.data2, aes(x="", y=area, fill=CropType)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0, direction = -1) +
    # geom_text(aes(y = ypos, label = prop), color = "black", size=3)+
    scale_fill_brewer(palette="RdYlGn") +
    theme(legend.position="none") +
    theme_void() # remove background, grid, numeric labels
gg.pie2
ggsave(filename = "CropTypes2015-2016_crops-only.png", plot = gg.pie2, scale = 1, path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes")

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

# change order & new plot-------------------------------------------------------

saveRDS(bar.data_complex, file = "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/bar_data_complex.RDS")


# create new var
bar = readRDS(file = "D:/Geodaten/#Jupiter/GEO402/03_develop/croptypes/bar_data_complex.RDS")

bar$CropType %>% unique()

RColorBrewer::display.brewer.all()

color.crops = RColorBrewer::brewer.pal(5, "Set2")
color.crops[5] = "#e8d543"

color.crops[4] = RColorBrewer::brewer.pal(4, "Set3")[4]
color.crops[2] = RColorBrewer::brewer.pal(9, "YlOrRd")[6]


vec = c("Sorghum", "Wheat", "SoyaBeans", "Sunflower", "Maize", "Fallow", "Pasture", "Unclassified")
colors = c(color.crops,"#9dbf82", "#1a870e", "grey30")

bar %>% print(n=50)

# change order
bar$CropType = factor(bar$CropType, levels = vec)

bar = bar %>% mutate(per = area/sum(bar$area, na.rm = TRUE))


myfont =

gg.bar3 = ggplot(bar) +
    geom_bar(aes(fill=CropType, y=area, x=year), position="fill", stat="identity") +
    scale_fill_manual(values = colors) +
    xlab("Crop Season") +
    ylab("Relative Area") +
    # geom_text(aes(label = percent(per)),position = position_fill())+
    scale_y_continuous(labels = percent_format()) +
    labs(fill='Land Use Classes') +
    theme_minimal(base_size = 20, base_family = "Gill Sans MT") +
    theme(axis.text.x = element_text(angle = 30, size = 10, vjust = 0.8),
          axis.text.y = element_text(angle = 0, size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12))
gg.bar3

# ggsave(plot = gg.bar1, filename = "CropTurnover_simple.png", path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes", scale = 1)
# ggsave(plot = gg.bar2, filename = "CropTurnover_complex.png", path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes", scale = 1)
ggsave(plot = gg.bar3, filename = "CropTurnover_complex_new.png", path = "D:/Geodaten/#Jupiter/GEO402/06_plots/Croptypes", scale = 1)

# Very good help page!!! GGPLOT Cheat sheet
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#change-size-of-tick-text-axis.text.x
# ------------------------------------------------------------------------------
