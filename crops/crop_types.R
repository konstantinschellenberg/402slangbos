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

options(max.print = 100)

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# LOAD DATA --------------------------------------------------------------------

crop1415 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "CropTypes_2014-2015")
crop1516 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "CropTypes 2015-2016")
crop1617 = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "CropTypes 2016-2017") %>%
    .[!.$CropType == "WheatMaize",]
crop_boundaries = read_sf("02_features/Ladybrand_CropData.gpkg", layer = "CropBoundaries2019")


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
names = c("CATNAME_1" ,"AREA_HA", "FIELD_ID", "CropType_1415", "CropType_1516", "CropType_1617", "geom")
names = c("CropType_1415", "CropType_1516", "CropType_1617")
join = crop_boundaries

for (i in seq_along(centroids)){
    print(i)
    join = st_join(join, centroids[[i]], join = st_contains)
    names(join)[length(names(join)) - 1] =  names[i]
}

# through out equal features
filtered = filter(join, !st_equals(join, sparse = FALSE)[1,])

st_write(filtered, "02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect", delete_layer = TRUE)

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
  select(4:6) %>%
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
classifiedNA = mutate(classified, description = paste(a, b, c, sep = "-"))

# SAME FOR NA CLEANED DATA -----------------------------------------------------

data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect") %>%
  na.omit()

ct = map(list(ct1 = unique(crop1415$CropType),
              ct2 = unique(crop1516$CropType),
              ct3 = unique(crop1617$CropType)),
         function(x) append(x, NA))

# get geometry column
sfc = st_geometry(data)

data = data %>%
  as.data.frame() %>%
  select(4:6) %>%
  map_df(function(x) as.factor(x))
print(data, n = 50)

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
count(classifiedNA, description, sort = TRUE) # with missing values
count(classified, description, sort = TRUE) # without missing values

st_write(classifiedNA, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplexWithNA", delete_layer = TRUE)
st_write(classified, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex", delete_layer = TRUE)

# Simplify the output ----------------------------------------------------------

data2 = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")
sfc = st_geometry(data2)
data2 = st_set_geometry(data2, NULL) %>%
  as_tibble()

# change column type to factor
data2[,1:3] = map(data2[1:3], function(x) as_factor(x))

check_identity = function(a, b){
  identical(as.character(a), as.character(b))
}

naming = function(x){
  case_when(x, x = "Pasture" ~ 1)
}

naming(data2)

identical(data2[1,1], data2[1,2])
paste(data2$a[1])
mutate(data2, i = identical(as.character(a), as.character(b)))
mutate(data2, identical = check_identity(a, b), .keep = "used") %>% print(n=50)
mutate(data2, identical = check_identity(a, b))




identical(as.character(data2$a[1]), as.character(data2$b[1]))
