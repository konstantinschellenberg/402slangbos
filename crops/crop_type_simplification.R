#' (2)
# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(leaflet)
library(leafem)
library(mapview)
library(TRAMPR)
library(ggplot2)

options(max.print = 100, )

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)


# LOAD PREVIOUS DATASETS -------------------------------------------------------

# Spatial Join of croptypes with boundary data ---------------------------------
crop.inter = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect") %>%
    replace_na(list(CropType_1415 = "X", CropType_1516 = "X", CropType_1617 = "X"))

# contains NA data of LandCare
crop.classif = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")

# OVERVIEW PLOTS ---------------------------------------------------------------

# counts of each category, `complex`
crop.classif %>%
    as.data.frame %>%
    count(description, sort = TRUE)

# counts of each category, `simple`

# CREATE NEW CLASSES AND CLASSIFY DATA -----------------------------------------

data = crop.inter

# get geometry column
sfc = st_geometry(data)

# fetch only cropdata columns for comparing with look-up table
data = data %>%
    as.data.frame() %>%
    select(1:3) %>%
    map_df(function(x) as.factor(x)) %>%
    `colnames<-`(c("a", "b", "c"))

# all possible crop changes as dataframe (land use change = luc)
# ct = croptypes possible
ct = rep(list(c("Pasture", "Crops", "Fallow", "X")), 3)

luc_cat = tidyr::expand_grid(a = ct[[1]], b = ct[[2]], c = ct[[3]]) %>%
    map_df(., ~ as.factor(.x))
print(luc_cat, n = 50)

# reclassify data
reclassify = function(x){
    case_when(x == "Maize" ~ "Crops",
              x == "Sunflower" ~ "Crops",
              x == "SoyaBeans" ~ "Crops",
              x == "Wheat" ~ "Crops",
              x == "Groundnuts" ~ "Crops",
              x == "Sorghum" ~ "Crops",
              x == "Pasture" ~ "Pasture",
              x == "Fallow" ~ "Fallow",
              x == "X" ~ "X")
}

data = map_df(data, function(x) reclassify(x)) %>% map_df(., ~ as.factor(.x))

# write out simplified categories
out = st_set_geometry(data, sfc)
st_write(out, "02_features/Ladybrand_CropData.gpkg", layer = "CropIntersectSimplified", append = FALSE)


# run classification
classified = cbind(data, cat = TRAMPR::classify(data, luc_cat))
classified = mutate(classified, description = paste(a, b, c, sep = "-"))

# write out simplified classification
out = st_set_geometry(classified, sfc)
st_write(out, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedSimplified", append = FALSE)

# CREATE TEMPORAL TRANSITION CLASSES -------------------------------------------

# TURNOVER CALCULATION RELOADED ------------------------------------------------
# now ignoring NA values
