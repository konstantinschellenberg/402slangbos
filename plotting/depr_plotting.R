#' Plotting execution
#'
#'
# Import Modules

source("import.R")

# Bulk plot creation -----------------------------------------------------------

for (a in 1:9){
    # loop through classes
    numbers = elements[a]
    cat("Printing class", a, "with", numbers, "reference plots\n")

    for (i in 31:numbers){
        # make bulk graphs and save to disk
        # for each gt in class

        # general outpath:
        outpath = paste0("D:/Geodaten/Master/projects/402slangbos/hiwi/", a, "/")
        dir.create(outpath)
        setwd(paste0(outpath))
        kuerzel = paste0(a, "-", i)

        # write out shp
        sf = position(a, i)
        st_write(sf, dsn = paste0(outpath, kuerzel, "contain.shp"), update = TRUE, quiet = TRUE)

        # title parsing
        tit = sf$descrip
        code = paste0("(code ",sf$Name,"/",sf$number,")")
        cat(code)

        # converting shp to zip archive
        archive.files = list.files(path = outpath, pattern = paste0(kuerzel, "contain"), full.names = T)
        zip::zipr(zipfile = paste0(kuerzel, "_shape.zip"), files = archive.files, include_directories = FALSE)
        file.remove(archive.files)

        # save map
        m = map(a, i)
        mapview::mapshot(m, file = paste0(outpath, kuerzel, "_map.png"),
                         remove_controls = c("zoomControl", "layersControl"))

        tryCatch(
            expr = {
                # Data contails all values
                # get stats
                stat = stats(list.dataframes, a, i, coherence_smoothing = TRUE)
                # plot calls
                p1 = plt1(stat, title = paste("VH Backscatter vs. VV Coherence,", tit, code))
                p2a = plt2(stat, scatter = TRUE, title = paste("VH Backscatter vs. VV Backscatter,", tit, code))
                # p2b = plt2(stat, scatter = FALSE, title = paste("VH Backscatter vs. VV Backscatter,", tit, code))
                # p3 = plt3(stat, title = paste("VH Coherence vs. VV Coherence,", tit, code))
                p4 = plt4(stat, title = paste("VV Backscatter vs. VV Coherence,", tit, code))

            },
            error = function(e){
                message("fatal error")
                message("\n", e)
                return(NA)},
            warning = function(w){
                # data contains NA values in the time series
                # get stats

                message(w)
                message("\nContinuing . . .")

                stat = stats(list.dataframes, a, i, coherence_smoothing = FALSE)

                # plot calls
                p1 <<- plt1(stat, all = FALSE, title = paste("VH Backscatter vs. VV Coherence,", tit, code))
                p2a <<- plt2(stat, scatter = TRUE, title = paste("VH Backscatter vs. VV Backscatter,", tit, code))
                # p2b <<- plt2(stat, scatter = FALSE, title = paste("VH Backscatter vs. VV Backscatter,", tit, code))
                # p3 <<- plt3(stat, all = FALSE, title = paste("VH Coherence vs. VV Coherence,", tit, code))
                p4 <<- plt4(stat, all = FALSE, title = paste("VV Backscatter vs. VV Coherence,", tit, code))

            }
        )
        plotly::orca(p1, file = paste0(kuerzel, "_bscVH-cohVV.svg"))
        plotly::orca(p2a, file = paste0(kuerzel, "_bscVH-bscVV.svg"))
        # plotly::orca(p2b, file = paste0(kuerzel, "_bscVH-bscVV_smooth.svg"))
        # plotly::orca(p3, file = paste0(kuerzel, "_cohVV-cohVH.svg"))
        plotly::orca(p4, file = paste0(kuerzel, "_bscVV-cohVV.svg"))

        # saving html
        htmlwidgets::saveWidget(widget = p1, file = paste0(outpath, kuerzel, "_bscVH-cohVV.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(widget = p2a, file = paste0(outpath, kuerzel, "_bscVH-bscVV.html"), selfcontained = TRUE)
        # htmlwidgets::saveWidget(widget = p2b, file = paste0(outpath, kuerzel, "_bscVH-bscVV_smooth.html"), selfcontained = TRUE)
        # htmlwidgets::saveWidget(widget = p3, file = paste0(outpath, kuerzel, "_cohVV-cohVH.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(widget = p4, file = paste0(outpath, kuerzel, "_bscVV-cohVV.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(widget = m, file = paste0(outpath, kuerzel, "_map.html"), selfcontained = TRUE)
        setwd(env)

    }
}


# bulk archiving ---------------------------------------------------------------

for (a in 1:length(elements)){
    setwd(env)
    numbers = elements[a]

    for (i in 1:numbers){
        kuerzel = paste0(a, "-", i, "_")
        code = paste0("(code ",a,"/",i,")")
        cat(kuerzel)

        # delete archive.zip
        file.remove(paste0("hiwi/", a, "/", kuerzel, "archive.zip"))

        # archive all plots in zip
        archive.files = list.files(path = "hiwi", pattern = kuerzel, full.names = T, recursive = T)
        archive.files = archive.files[!grepl(x = archive.files,"_archive")]
        print(archive.files)
        zip::zipr(zipfile = paste0("hiwi/", a, "/", kuerzel, "archive.zip"), files = archive.files, include_directories = FALSE)
    }
}

# copy example plots to storage directory --------------------------------------

example_plots = c("1-7",
                  "1-12",
                  "1-16",
                  "1-26",
                  "1-32",

                  "2-8",
                  "2-17",
                  "2-22",
                  "2-31",
                  "2-32",

                  "3-2",
                  "3-4",
                  "3-8",
                  "3-11",
                  "3-12",

                  "4-1",
                  "4-3",
                  "4-4",
                  "4-12",

                  "6-2",
                  "6-4",
                  "6-7",
                  "6-13",

                  "7-5",
                  "7-9")

######## declare new paths
all = list.files("D:/Geodaten/Master/projects/402slangbos/hiwi", recursive = TRUE, full.names = TRUE)
new_path = "third_05_2020"
########

for (i in example_plots){
    pattern = paste0(i, "_archive.zip")
    copy.from = all[grepl(pattern, all)]
    print(copy.from)

    path = paste0("D:/Geodaten/#Jupiter/GEO402/HIWI_Documents/exploration/", new_path, "/#examples/")
    dir.create(path, recursive = TRUE)
    file.copy(from = copy.from, to = path, overwrite = TRUE)
    zip::unzip(paste0(path, pattern), overwrite = TRUE, exdir = paste0(path, "unzipped/"))

}


# Copy all other plots in storage ----------------------------------------------

for (i in all){
    copy.from = i[grepl("_archive", i)]
    print(copy.from)

    path = paste0("D:/Geodaten/#Jupiter/GEO402/HIWI_Documents/exploration/", new_path)
    file.copy(from = copy.from, to = path, overwrite = TRUE)
}

