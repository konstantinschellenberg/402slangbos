# init 09.09.2020
# bandname creation for SLC/Interferometric Coherences


# CREATE DATES FROM HDR FILE ---------------------------------------------------

headers = "D:/Geodaten/GEO402/01_data/S1_SLC/S1_A_VV_stack_coherence_full_area.hdr"
prefix = "co"

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

names(stack_bandnames) = "x"

write.csv(stack_bandnames, file = "D:/Geodaten/GEO402/01_data/S1_SLC/S1_A_VV_stack_coherence_full_area.txt", row.names = FALSE)

# CREATE DATES FROM VRT --------------------------------------------------------

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
