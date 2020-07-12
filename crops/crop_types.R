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
# not really necessary...
#'
#'
#'
#'

# data = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropIntersect") %>%
#   na.omit()
#
# ct = map(list(ct1 = unique(crop1415$CropType),
#               ct2 = unique(crop1516$CropType),
#               ct3 = unique(crop1617$CropType)),
#          function(x) append(x, NA))
#
# # get geometry column
# sfc = st_geometry(data)
#
# data = data %>%
#   as.data.frame() %>%
#   select(4:6) %>%
#   map_df(function(x) as.factor(x))
# print(data, n = 50)
#
# # all possible crop changes as dataframe (land use change = luc)
# luc_cat = tidyr::expand_grid(a = ct[[1]], b = ct[[2]], c = ct[[3]])
# print(luc_cat, n = 50)
#
# table_real = data %>%
#   `colnames<-`(c("a", "b", "c"))
# use_real = luc_cat %>%
#   map_df(., ~ as.factor(.x))
#
# # run classification
# classified = cbind(table_real, cat = TRAMPR::classify(table_real, use_real))
# st_geometry(classified) = sfc
#
# # add description column
# classified = mutate(classified, description = paste(a, b, c, sep = "-"))

#'
#'
#'
#'
#'

# RESULTS ----------------------------------------------------------------------
count(classifiedNA, description, sort = TRUE) %>% print(n=50) # with missing values
count(classified, description, sort = TRUE) # without missing values

st_write(classifiedNA, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplexWithNA", delete_layer = TRUE)
st_write(classified, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex", delete_layer = TRUE)

# Simplify the output ----------------------------------------------------------

data2 = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplex")
data2 = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedComplexWithNA")

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

st_write(data2, "02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnoversNA", delete_layer = TRUE)

# PLOTTING ---------------------------------------------------------------------

data3.full = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnovers")
data3.na = st_read("02_features/Ladybrand_CropData.gpkg", layer = "CropClassifiedTurnoversNA")
sfc = st_geometry(data3)
data3 = st_set_geometry(data3, NULL) %>%
  as_tibble()

library(grid)
library(gridExtra)


data3 = list(data3.full, data3.na)
sfc = map(data3, ~ st_geometry(.x))
data3 = map(data3, ~ as_tibble(st_set_geometry(.x, NULL)))

g1 = map(data3, function(x){
  ggplot(x) +
    geom_bar(aes(turnover)) +
    theme_bw()
})

facet1 = grid.arrange(g1[[1]], g1[[2]], nrow = 2)
ggsave(plot = facet1, filename = "turnover_amount.png", path = "HIWI_Documents/agriculture/", scale = 1.5)


ggplot(data3) +
  geom_bar(aes(turnover))


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
ggsave(plot = facet2, filename = "turnover.png", path = "HIWI_Documents/agriculture/")
