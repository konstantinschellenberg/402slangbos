# init 09.09.2020
# bandname creation for GRD

source("D:/Geodaten/Master/projects/402slangbos/import.R")

# LOAD RASTERS -----------------------------------------------------------------

headers = c("F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack.hdr",
         "F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack.hdr")

# TODO: Only keep layers with extent

ext = list(c(441536.3670000000274740,6679728.9309999998658895),
           c(553596.3670000000856817,6797548.9309999998658895))


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
