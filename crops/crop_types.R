#' crop type analysis
#' init: 09.07.2020, Konstantin Schellenberg
#' Intersection of crop types per year


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

# LOAD DATA --------------------------------------------------------------------

crop1415 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "full_CropTypes_2014-2015")
crop1516 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "full_CropTypes_2015-2016")
crop1617 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "full_CropTypes_2016-2017") %>%
    .[!.$CropType == "WheatMaize",]
crop_boundaries = st_geometry(crop1617) %>% st_as_sf()


# fetch column
crops = map(list(crop1415, crop1516, crop1617), function(x) select(x, CropType))


# PLOT & INFORMATION -----------------------------------------------------------
# existing crop types

ct = map(list(ct1 = unique(crop1415$CropType),
              ct2 = unique(crop1516$CropType),
              ct3 = unique(crop1617$CropType)),
         function(x) append(x, "X"))


diff = list(setdiff(ct$ct1, ct$ct2), setdiff(ct$ct2, ct$ct3), setdiff(ct$ct1, ct$ct3))
print(diff)

# plotting
# mapview(crop1415, zcol = "CropType")

map(crops, function(x) plot(x))

# number of observations:
map(crops, function(x) nrow(x))

# ANALYSIS ---------------------------------------------------------------------
# Real Data

centroids = map(crops, function(x) st_centroid(x))
names = c("CropType_1415", "CropType_1516", "CropType_1617")
join = crop_boundaries

for (i in seq_along(centroids)){
    print(i)
    join = st_join(join, centroids[[i]], join = st_contains)
    names(join)[length(names(join)) - 1] =  names[i]
}

# through out equal features
filtered = filter(join, !st_equals(join, sparse = FALSE)[1,])

st_write(filtered, "02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect", append = FALSE)

# COUNT OF LAND USE TURNOVER ---------------------------------------------------
#LOAD --------------------------------------------------------------------------

# WITH NA ----------------------------------------------------------------------
data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect") %>%
  replace_na(list(CropType_1415 = "X", CropType_1516 = "X", CropType_1617 = "X"))

ct = map(list(ct1 = unique(crop1415$CropType),
              ct2 = unique(crop1516$CropType),
              ct3 = unique(crop1617$CropType)),
         function(x) append(x, "X"))

# get geometry column
sfc = st_geometry(data)

data = data %>%
  as.data.frame() %>%
  select(contains("CropType")) %>%
  map_df(function(x) as.factor(x))

# all possible crop changes as dataframe (land use change = luc)
luc_cat = tidyr::expand_grid(a = ct[[1]], b = ct[[2]], c = ct[[3]])
print(luc_cat, n = 50)

table_real = data %>%
  `colnames<-`(c("a", "b", "c"))
use_real = luc_cat %>%
  map_df(., ~ as.factor(.x))

# run classification
classified = cbind(table_real, cat = TRAMPR::classify(table_real, use_real))
st_geometry(classified) = sfc

# add description column
classified = mutate(classified, description = paste(a, b, c, sep = "-"))

# RESULTS ----------------------------------------------------------------------
count(classified, description, sort = TRUE) # without missing values

st_write(classified, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex", append = FALSE)

# Simplify the output ----------------------------------------------------------

data2 = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")

sfc = st_geometry(data2)
data2 = st_set_geometry(data2, NULL) %>%
  as_tibble()

renaming = function(x){
  case_when(x == "Pasture" ~ 1,
              x == "Fallow" ~ 2,
              x == "Maize" ~ 3,
              x == "Sunflower" ~ 4,
              x == "SoyaBeans" ~ 5,
              x == "Wheat" ~ 6,
              x == "Groundnuts" ~ 7,
              x == "Sorghum" ~ 8,
              x == "X" ~ 0)
}

data2[1:3] = map_df(data2[1:3], ~ renaming(.x))

data2 %>%
  mutate(count = n(cat))

data2 = data2 %>%
  mutate(turnover = as.numeric(a!=b) + as.numeric(b!=c)) %>%
  st_set_geometry(sfc)

st_write(data2, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers", append = FALSE)
