# init 09.09.2020
# bandname creation for GRD

source("D:/Geodaten/Master/projects/402slangbos/import.R")

dir = c("F:/geodata/geo402/S1_SLC/xx_new/VRTs")

# CREATE DATES -------------------------------------------------------------------

stack_bandnames = map(dir, function(x){

    # get file names
    files = list.files(x, full.names = FALSE, pattern = "*.vrt$")

    # substring date
    phrase.datum = stringr::str_sub(files, start = 10, end = 17)

    # convert to POSTict (R-date) format
    phrase.date = as.Date(phrase.datum, format = "%Y%m%d") %>%
        as.character() %>%
        stringr::str_replace_all("-", ".")

    # prepend the prefix to date information
    bandnames = map_chr(phrase.date, function(x) paste0("co", ".", x))

})

# create bandnames .txt
map2("F:/geodata/geo402/S1_SLC/xx_new/S1A_IW_SLC_VV_stack", stack_bandnames, function(x, y){
    outfile = paste0(x, ".txt")

    write.csv(y, outfile, row.names = FALSE)
})
