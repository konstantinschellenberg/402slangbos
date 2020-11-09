# Maske vorprozessieren

source("D:/Projects/402slangbos/import_rasters_classification.R")

# ANALYSIS ---------------------------------------------------------------------

# load polygon
maske = st_read("D:/Geodaten/GEO402/02_features/study_area.gpkg", layer = "mask")

# RASTERISATION to coherence path layer
mask = raster::rasterize(maske, cube[[25]], background = 0)

writeRaster(mask, filename = "D:/Geodaten/GEO402/01_data/mask.tif", overwrite = TRUE)
