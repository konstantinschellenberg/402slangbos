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

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# SET ENVIRONMENT --------------------------------------------------------------

env = "D:/Geodaten/#Jupiter/GEO402"
setwd(env)

# complex or simplified data?
# 1 = selected classes, 2 = all sites
mode = 2

# be aware of using the gt or cgt samples data accordingly!

# SOME METADATA  ----------------------------------------------------

# metadata
col_class = "class_simple"
col_id = "id"

if (mode == 1) dstdir = "03_develop/extract/"
if (mode == 2) dstdir = "03_develop/extract_all/"

# layernames of raster need to have a date suffix in the form yyyy.mm.dd

outfiles = map(layernames, function(x) paste("extract", x, sep = "_"))
outfile = "test"

# median is calculated anyways; must be coercable by exact_extract()
statistics = c("mean", "stdev", "count")

# EXTRACTING -------------------------------------------------------------------
if (mode == 1) b = exactextracting(gt, vh[[1:10]], col_class, col_id, statistics, dstdir, "outfile")
if (mode == 2) b = exactextracting(cgt, vh[[1:10]], col_class, col_id, statistics, dstdir, "outfile")

ex = b[[8]][[1]]
ggplot(ex, aes(date, med)) +
    geom_point()

if (mode == 1){

map2(outfiles, rasters, function(x, y){
    cat(x)
    exactextracting(gt = gt, ras = y,
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = x)})
} else if (mode == 2){

map2(outfiles, rasters, function(x, y){
    cat(x)
    exactextracting(gt = cgt, ras = y,
                    col_class = "class_simple",
                    col_id = "id",
                    statistics = statistics,
                    dstdir = dstdir,
                    outfile = x)})
}


# reip 3-15 PROBLEM


# READ IN ----------------------------------------------------------------------

# concat file paths
path.extract = list.files("03_develop/extract", pattern = "^extract") %>%
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
