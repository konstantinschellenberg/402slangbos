#' (3)
# Intersect Crop Types and Turnover scores with Slangbos samples

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)

options(max.print = 100)

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# LOAD DATASETS ----------------------------------------------------------------

data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplified")
#turnover = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers")
gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple")
# gt$break_date = as.POSIXct(gt$break_date, format = "%m%Y", optional = T)

# PERFORM INTERSECTION ---------------------------------------------------------

renaming = function(x){
    case_when(x == "Pasture" ~ 1,
              x == "Fallow" ~ 2,
              x == "Crops" ~ 3,
              x == "X" ~ 0)
}

sfc = st_geometry(data)

data = data %>% st_drop_geometry() %>% as.data.frame()


# index the crop type columns
data[1:4] = map_df(data[1:4], ~ renaming(.x))
data[data == 0] = NA

# data = sample_n(data, 100) %>% print
# data[[1,1]] = NA

# turnover = data %>%
#     mutate(turnover = case_when(!is.na(a&b&c&d) ~ as.numeric(a!=b) + as.numeric(b!=c) + as.numeric(c!=d),
#                                 is.na(a) ~ as.numeric(b!=c),
#                                 is.na(b) ~ as.numeric(a!=c),
#                                 is.na(c) ~ as.numeric(a!=b),
#                                 is.na(d) ~ as.numeric(d))) %>%
    # st_set_geometry(sfc)

turnover = data %>%
    mutate(turnover = as.numeric(a!=b) + as.numeric(b!=c) + as.numeric(c!=d)) %>%
    st_set_geometry(sfc)

st_write(turnover, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplifiedTurnover", append = FALSE)

inter = st_join(gt, turnover, join = st_intersects)

st_write(inter, "02_features/Ladybrand_CropData.gpkg", layer = "SampleIntersection", append = FALSE)

# now visualize crop type in sentinel timeseries...
