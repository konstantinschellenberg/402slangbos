# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

# Script to import and clean data

library(tidyverse)
library(raster)
library(purrr)
library(sf)

################################################################################
# Import Ground Truth-----------------------------------------------------------
################################################################################

study_area = st_read("D:/Geodaten/GEO402/02_features/study_area.gpkg", layer = "final", quiet = TRUE) %>%  # read in
    st_zm(drop = TRUE)  # Remove Z-Dimension

gt = st_read("D:/Geodaten/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_simple") %>%
    st_zm() %>%
    group_by(class_simple) %>%
    # CREATE RUNNING NUMBERS FOR GT
    mutate(id = row_number()) %>%
    dplyr::ungroup()

cgt = st_read("D:/Geodaten/GEO402/02_features/features.gpkg", layer = "LADYBRAND_gt_stats_full_simple") %>%
    st_zm() %>%
    group_by(class_simple) %>%
    # CREATE RUNNING NUMBERS FOR GT
    mutate(id = row_number()) %>%
    dplyr::ungroup()
