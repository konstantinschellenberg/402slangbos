# init 09.09.2020
# bandname creation for GRD

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# LOAD RASTERS -----------------------------------------------------------------

headers = c("F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack.hdr",
         "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack.hdr")

prefix = c("vh", "vv")

# TODO: Only keep layers with extent

ext = list(c(441536.3670000000274740,6679728.9309999998658895),
           c(553596.3670000000856817,6797548.9309999998658895))

# FILTER ONLY PATH 14, not PATH 116:
dir = "F:/geodata/geo402/S1_GRD/xx_new/GRD_VV/"
dir = "F:/geodata/geo402/S1_GRD/xx_new/GRD_VH/"

ras1 = raster("F:/geodata/geo402/S1_GRD/xx_new/GRD_VH/S1A__IW___A_20150311T165324_VH_grd_mli_norm_geo_db.tif")
ras2 = raster("F:/geodata/geo402/S1_GRD/xx_new/GRD_VH/S1A__IW___A_20150323T165351_VH_grd_mli_norm_geo_db.tif")

extent(ras1)
extent(ras2)

# try to discriminate layers by extent (because metainformation is gone...)
layers = list.files(dir, full.names = TRUE)

# get metainformation about raster origin, base for comparison
origin = map(layers, function(x){
    origin.raw = gdalinfo(x)[44]
    origin.splitted = origin.raw %>% str_split(., pattern = " ")
    origin.coords = origin.splitted[[1]][3]
    origin = origin.coords %>%
        str_remove(pattern = "\\(") %>%
        str_remove(pattern = "\\)") %>%
        str_split(pattern = ",") %>%
         .[[1]] %>%
        as.numeric()
})

# when raster origin matches reference origin, put raster path in list
# master list
rastermeta = map2(layers, origin, ~ c(path = .x, origin = list(.y)))

reference = rastermeta[[5]]
testslave = origin[[1]]

# rastermeta = rastermeta[1:10]

matches = map(rastermeta, function(x){
    lst = list()
    if (identical(reference$origin, x$origin)) {
        lst = append(lst, x)
    }
})

# reduce empty lists
matches.comp = compact(matches)

# write out paths
matches.paths = map_chr(matches.comp, ~ .x[["path"]])

# number of GRD layers kept:
length(matches.paths) %>% cat

#
outdir = "F:/geodata/geo402/S1_GRD/xx_new/GRD_VV_vrts/"
outdir = "F:/geodata/geo402/S1_GRD/xx_new/GRD_VH_vrts/"

for (i in seq_along(matches.paths)){
    print(i)

    # concat outfile name
    splitted = matches.paths[i] %>% str_split(pattern = "/")
    name = splitted[[1]][7]

    # erase ".tif"
    name = name %>% str_remove(pattern = ".tif")
    output = paste0(outdir, name, ".vrt")
    print(output)

    stack.dir = paste0(files.mosaic[i], "/*.tif")
    # scenes = list.files(stack.dir, full.names = TRUE)

    command = sprintf("gdalbuildvrt -separate -srcnodata -99 -vrtnodata -99 -overwrite %s %s", output, matches.paths[i])
    system(command)
}

# CREATE DATES -------------------------------------------------------------------
stack_bandnames = map2(headers, prefix, function(x, y){


    # hdr file read ------------------------------------------------------------
    bandnames = read.delim(x, header = TRUE, sep = "=") %>%
        .[["band names",1]]

    # remove braces at the beginning and end
    brace = str_remove_all(bandnames, pattern = "[{}]")

    # trim whitespaces at the beginning
    trimmed = trimws(brace, which = "both")

    # split by commata
    splitted = str_split(trimmed, pattern = ", ")

    # bandnames ----------------------------------------------------------------
    # substring date
    phrase.datum = stringr::str_sub(splitted[[1]], start = 13, end = 20)

    # convert to POSTict (R-date) format
    phrase.date = as.Date(phrase.datum, format = "%Y%m%d") %>%
        as.character() %>%
        stringr::str_replace_all("-", ".")

    # prepend the prefix to date information
    map_chr(phrase.date, function(x) paste0(y, ".", x))

})

# create bandnames .txt
map2(headers, stack_bandnames, function(x, y){

    split = strsplit(x, split = "\\.") %>% .[[1]]
    outfile = paste0(split[1], ".txt")

    write.csv(y, outfile, row.names = FALSE)
})
