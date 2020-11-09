# splitting the gt data into years


# setwd("D:/Geodaten/Projects/402slangbos")
source("import_samples.R")
source("functions.R")

library(mapview)
library(exactextractr)
library(leaflet)

#' for classification keep
#' 1: Slangbos
#' 2: Pasture
#' 3: Grassland
#'
#' 4: Cultivated
#' 5: Bare Soil
#' 6: Woodland
#' 7: Water
#' 8: Urban

# IMPORT SAMPLES WITH ALL CLASSES ----------------------------------------------

cl1718.in = st_read("D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "pre_mapping", quiet = TRUE)

cl1718 = cl1718.in %>%
    mutate(classnames = case_when(classif == 1 ~ "Slangbos",
                                  classif == 2 ~ "Pasture",
                                  classif == 3 ~ "Grassland",
                                  classif == 4 ~ "Cultivated",
                                  classif == 5 ~ "Bare",
                                  classif == 6 ~ "Woodland",
                                  classif == 7 ~ "Urban",
                                  classif == 8 ~ "Water")) %>%
    dplyr::select(classif, classnames) %>%
    mutate_at("classif", factor)

print(cl1718, n = 100)
mapview(cl1718)


# here is space for the other years!



# FILTER BY TOPOGRAPHY ---------------------------------------------------------

classif = cl1718

p.slope = "D:/Geodaten/GEO402/01_data/srtm/Slope10.tif"
slope = raster(p.slope)

masked = exactextractr::exact_extract(slope, classif, "median")
sum(!is.na(masked))
cat("Samples in slopy areas:", sum(!is.na(masked)))

classif.masked = classif %>%
    mutate(mask = masked) %>%
    filter(is.na(mask)) %>%
    dplyr::select(-mask)


# FILTER BY SLC PATH ---------------------------------------------------------

slcpolygon = st_read("D:/Geodaten/GEO402/02_features/study_area.gpkg", "SLC_Path_outline_Polygon")
st_difference(classif.masked, slcpolygon)

# check if layers are valid
slcpolygon %>% st_is_valid()
classif.masked %>% st_is_valid()

# create sparse matrix
mat = st_contains_properly(slcpolygon, classif.masked, sparse = F)
cat("Samples outside of the path:", sum(mat == FALSE))

# select samples inside the path polygon
classif.out = classif.masked[c(mat),]

mapview(classif.out)

# OUTPUT -----------------------------------------------------------------------

st_write(classif.out, "D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "classif_2017-2018", append = FALSE)


# VISO -------------------------------------------------------------------------

leaflet() %>%
    addProviderTiles("Stamen.TonerHybrid", group = "Stamen.TonerHybrid") %>%
    # addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
    addPolygons(data = st_transform(classif, 4326),
                fillOpacity = 1,
                weight = 1,
                color = "black") %>%
    addPolygons(data = st_transform(classif.masked, 4326),
                fillOpacity = 1,
                weight = 1,
                color = "red")

# Just once!! MERGE PASTURE CLASS WITH GRASSLAND -------------------------------

# sf = st_read("D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "classif_2017-2018")
#
# # merge classes by renaming
# sf = sf %>%
#     mutate(classif = case_when(classif == 3 ~ 2,
#                      classif == 4 ~ 3,
#                      classif == 5 ~ 4,
#                      classif == 6 ~ 5,
#                      classif == 7 ~ 6,
#                      classif == 8 ~ 7,
#                      TRUE ~ as.numeric(classif)))
# # renaming
# sf = sf %>%
#     mutate(classnames = case_when(classif == 1 ~ "Slangbos",
#                                          classif == 2 ~ "Grassland",
#                                          classif == 3 ~ "Cultivated",
#                                          classif == 4 ~ "Bare",
#                                          classif == 5 ~ "Woodland",
#                                          classif == 6 ~ "Urban",
#                                          classif == 7 ~ "Water")) %>%
#     dplyr::select(classif, classnames) %>%
#     mutate_at("classif", factor)
#
# sf$classnames %>% unique()
#
# st_write(sf, "D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "classif_2017-2018", append = FALSE)
