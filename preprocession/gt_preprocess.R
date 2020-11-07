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

gt = st_read("02_features/features.gpkg", layer = "LADYBRAND_all_samples") %>%
    st_zm()

# follow here when having added new ground samplings and want to add to the database
# new = st_read("02_features/2020-07-20-new_polygons.kml")
#
# # replace classes by names of classes
# new = new %>%
#     transmute(new_class = case_when(Name == "11" ~ "slangbos_increase_oldfield",
#                                  Name == "12" ~ "slangbos_increase_grassland",
#                                  Name == "21" ~ "slangbos_continuous_oldfield",
#                                  Name == "22" ~ "slangbos_continuous_grassland",
#                                  Name == "31" ~ "slangbos_break-crop",
#                                  Name == "32" ~ "slangbos_break-pasture",
#                                  Name == "4" ~ "grassland",
#                                  Name == "5" ~ "cultivated",
#                                  Name == "6" ~ "bare",
#                                  Name == "7" ~ "woodland",
#                                  Name == "8" ~ "urban",
#                                  Name == "9" ~ "water",
#                                  Name == "7" ~ "eucalyptus")) %>%
#     st_transform(st_crs(gt)) %>%
#     st_zm()
#
# st_write(new, "02_features/features.gpkg", layer = "LADYBRAND_new_samples", overwrite = T)

# jetzt Vektor zusammenführen in QGIS manuell durchführen... und zu LADYBRAND_all_samples hinzufügen

# RECLASS ----------------------------------------------------------------------

# classify
gt_full = gt %>%
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

gt_full = gt_full %>% dplyr::select(c(class, new_class, break_date, checked_name))
st_write(gt_full, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full", append = FALSE)


# ------------------------------------------------------------------------------
# filter for Slangbos analysis, not classification (drop woody, water, urban and bare)
gt_stats = filter(gt_full, class != "6", class != "7", class != "8", class != "9")
print(gt_stats, n =50)

st_write(gt_stats, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_complex", append = FALSE)


# even simplified further ------------------------------------------------------
gt_stats_simplified = mutate(gt_stats, class_simple = case_when(class = str_detect(class, "^1") ~ 1,
                                                       class = str_detect(class, "^2") ~ 2,
                                                       class = str_detect(class, "^3") ~ 3,
                                                       class == "4" ~ 4,
                                                       class == "5" ~ 5))


st_write(gt_stats_simplified, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple", append = FALSE)


# making gt for all classes, but not slangbos subclasses: ----------------------
gt_full_simplified = gt_full %>%
    mutate(class_simple = case_when(class = str_detect(class, "^1") ~ 1,
                                    class = str_detect(class, "^2") ~ 2,
                                    class = str_detect(class, "^3") ~ 3,
                                    class == "4" ~ 4,
                                    class == "5" ~ 5,
                                    TRUE ~ as.numeric(class))) %>%
    mutate_at("class_simple", as.character()) %>%
    mutate(classnames = case_when(class_simple == "1" ~ "Slangbos Increase",
                                  class_simple == "2" ~ "Slangbos Continuous",
                                  class_simple == "3" ~ "Slangbos Clearning",
                                  class_simple == "4" ~ "Grassland",
                                  class_simple == "5" ~ "Cultivated",
                                  class_simple == "6" ~ "Bare Soil",
                                  class_simple == "7" ~ "Woodland",
                                  class_simple == "8" ~ "Urban",
                                  class_simple == "9" ~ "Water")) %>%
    mutate_at("classnames", as.factor) %>%

# double-check final classes:
unique(gt_full_simplified$class_simple) %>% sort
st_write(gt_full_simplified, "02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full_simple", append = FALSE)

# ------------------------------------------------------------------------------

# print count of each class
count(gt, class)
ggplot(gt) +
    geom_bar(aes(new_class)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

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

count(gt_full_simplified, class_simple)
ggplot(gt_full_simplified) +
    geom_bar(aes(classnames)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

# end
