# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(mapview)
library(ggplot2)

options(max.print = 100, )

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# LOAD DATA --------------------------------------------------------------------

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_all_samples")

# RECLASS ----------------------------------------------------------------------

# classify
gt = gt %>%
    filter(stats_use == TRUE) %>%
    mutate(class =  case_when(new_class == "slangbos_increase_oldfield" ~ "11",
                              new_class == "slangbos_increase_grassland" ~ "12",
                              new_class == "slangbos_continuous_oldfield" ~ "21",
                              new_class == "slangbos_continuous_grassland" ~ "22",
                              new_class == "slangbos_break-crop" ~ "31",
                              new_class == "slangbos_break-pasture" ~ "32",
                              new_class == "grassland" ~ "4",
                              new_class == "cultivated" ~ "5",
                              new_class == "bare" ~ "6",
                              new_class == "woodland" ~ "7",
                              new_class == "urban" ~ "8",
                              new_class == "water" ~ "9",
                              new_class == "eucalyptus" ~ "7"))

st_write(gt, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full", overwrite = TRUE)

# filter for Slangbos analysis, not classification (drop woody, water, urban and bare)
gt_stats = filter(gt, class != "6", class != "7", class != "8", class != "9") %>%
    dplyr::select(c(class, new_class, break_date, checked_name))

print(gt_stats, n =50)

st_write(gt_stats, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_complex")

# even simplified further
gt_stats_simplified = mutate(gt_stats, class_simple = case_when(class = str_detect(class, "^1") ~ 1,
                                                       class = str_detect(class, "^2") ~ 2,
                                                       class = str_detect(class, "^3") ~ 3,
                                                       class == "4" ~ 4,
                                                       class == "5" ~ 5))


st_write(gt_stats_simplified, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple")

# print count of each class
count(gt_stats, class)
ggplot(gt_stats) +
    geom_bar(aes(new_class)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

count(gt_stats_simplified, class_simple)
ggplot(gt_stats_simplified) +
    geom_bar(aes(class_simple)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
