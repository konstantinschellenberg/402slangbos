#' Scrip for extracting sample data from all sensors and for all classes within
#' the SALDi-Slangbos project
#' saves the data sensor-wise (1) and coerces everything in a tidy dataframe (2)

# LOAD PACKAGES ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(plotly)

library(exactextractr)

options(max.print = 200)

source("D:/Projects/402slangbos/import_rasters_extraction.R")
source("D:/Projects/402slangbos/functions.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/GEO402"
setwd(env)

# be aware of using the gt or cgt samples data accordingly!

# SOME METADATA  ----------------------------------------------------

# get gt object
sample = st_read("D:/Geodaten/GEO402/02_features/classif.gpkg", layer = "classif_2017-2018") %>%
    group_by(classif) %>%
    mutate(id = row_number())

# metadata
col_class = "classif"
col_id = "id"

dstdir = "03_develop/extract_classif"
if (!dir.exists(dstdir)) dir.create(dstdir)

# layernames of raster need to have a date suffix in the form yyyy.mm.dd
outfiles = map(layernames, function(x) paste("extract", x, sep = "_"))
outfile = "test"

# median is calculated anyways; must be coercable by exact_extract()
statistics = c("median", "mean", "stdev", "count")

# EXTRACTING -------------------------------------------------------------------
b = exactextracting(sample, vh[[1:10]], col_class, col_id, statistics, dstdir, "outfile")
# b = readRDS(paste0(dstdir, "outfile"))

ex = b[[8]][[1]]
ggplot(ex, aes(date, median)) +
    geom_point()

map2(outfiles, rasters, function(x, y){
    cat(x)
    exactextracting(gt = sample, ras = y,
                    col_class = "classif",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = x)})


# READ IN ----------------------------------------------------------------------

# concat file paths
path.extract = list.files(dstdir, pattern = "^extract") %>%
    file.path("03_develop", "extract", .)
path.extract = path.extract[!grepl("all.RDS", path.extract)]

data.raw = map(path.extract, ~ readRDS(.x))

# rename list headers
data = data.raw %>%
    `names<-`(layernames) %>%
    map( ~ `names<-`(.x, classnames))

# TIDY TABLE LONG CREATION -----------------------------------------------------

# sensor -> class -> sample ----------------------------------------------------
a = data$vh$`Slangbos Increase`$`1`
b = data$vh$`Slangbos Increase`$`2`

# sensor -> class --------------------------------------------------------------

# dplyr
# df = map_df(data$vh$`Slangbos Increase`, ~ rbindlist(.x, use.names = TRUE, idcol = TRUE))
# df2 = map_df(data$vh$`Slangbos Continuous`, ~ bind_rows(.x))

# data.table -> perferred!
df = data$vh$`Slangbos Increase` %>% rbindlist(idcol = TRUE) %>% rename("sample" = .id)
df2 = data$vh$`Slangbos Continuous` %>% rbindlist(idcol = TRUE) %>% rename("sample" = .id)

# each contains the data of a sensor & a class, binded a rows without ids

# sensor -----------------------------------------------------------------------
# data.table solution
# s.vh = map(data$vh, ~ rbindlist(.x, use.names = TRUE, idcol = "sample")) %>%
#     rbindlist(., use.names = TRUE, idcol = "class")
# s.ndvi = map(data$ndvi, ~ rbindlist(.x, use.names = TRUE, idcol = TRUE)) %>%
#     rbindlist(., use.names = TRUE, idcol = "class")

sensors = map(data, function(x){
    classes = map(x, ~ rbindlist(.x, use.names = TRUE, idcol = "sample"))
    sensors = rbindlist(classes, use.names = TRUE, idcol = "class")
    return(sensors)
})

# scale up to master level -> one tidy dataframe
master = rbindlist(sensors, use.names = TRUE, idcol = "sensor")

# reorder columns, convert "sample" character to integer and sort after "date"
master = master %>% relocate("date", where(is.character), .before = where(is.numeric)) %>%
    mutate(across(.cols = sample, as.integer)) %>%
    arrange(date, sensor, class, sample)

# now, we have the tidy dataframe for all stats!!
write_rds(master, "03_develop/extract/extract_all.RDS")
