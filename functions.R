# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# Supervisor: Dr. Marcel Urban

# This script serves to read Sentinel-1 Radar time series in order to extract pixel values falling in the ground truth training
# and validation polygons (GT)
# Functions are written to simplify the usage and enhance debugging
#
# 1. rename_bandnames:
# function to retrieve the dates from the sentinel-1 scene providid by Marcel Urban.
# Dates are subsequently written to the layer names of the raster brick.
#
# 2. gt_from_raster: credits to http://amsantac.co/blog/en/2015/11/28/classification-r.html
# Does the same as carve_brick & list_summaries. It output two datasets:
# The first: "learning_input.rds", dataframe with variables as columns. Serves as input for machine learning
# The second: "gt_list.rds", list of all the polyon information in the time stack. Stores in nested lists and dataframe
#
# 3. carve_brick:
# Reads s1 and gt, extracts "carves" the polygons in the raster brick and calculates some basic stats (mean, median, stdev)
#
# 4. list_summaries:
# Takes s1 and gt data, calls carve_brick and appends each retrieved dataframe-summary to a created list "summary"


# Prerequisits:
# Sentinel-1 time-stack
# Polygons of ground truth with the follwing column:
# name
# can have categories:
# Slangbos-Classes
# 1 = Increase
# 2 = Continuous
# 3 = Break
#
# andere
# 4 = Agriculture
# 5 = bare soil (gibt es kaum, wahrscheinlich invalide)
# 6 = grassland
# 7 = forest canopy
# 8 = urban
# 9 = water

################################################################################
# Clips on the desired threshold fraction which is allowed to be obstructed on
# raster -----------------------------------------------------------------------
################################################################################

fraction_cleaner = function(ras, thresh = 0.2){

    raslist = list()
    for (i in 1:nlayers(ras)){
        r = ras[[i]]
        raslist = append(raslist, r)
    }

    ncells = map(raslist, ~ ncell(.x))

    tables = map(raslist, ~ table(is.na(.x[])))
    na.frac = map2(tables, ncells, function(x, n) as.numeric(x[2]) / n)

    cat("fraction of NA pixels in images:\n")
    cat(round(as.numeric(na.frac), digits = 2), sep = "\t")
    cat("Threshold is:", thresh)
    cat("\n")

    keep = ras[[which(na.frac < thresh)]]

    cat("Total Layers:", nlayers(ras), "\n")
    cat("Obstructed Layers:", nlayers(ras) - nlayers(keep), "\n")
    cat("Surviving Layers:", nlayers(keep), "\n")
    return(keep)
}

################################################################################
# BANDNAME FILE CREATION -------------------------------------------------------
################################################################################

bandnames = function(file, prefix = NULL, writeout = FALSE){

    # load raster
    ras = brick(file)
    phrase = names(ras) # get names

    # Substring
    phrase.datum = substr(phrase, start = 12, stop = 19)

    # convert to POSTict (R-date) format
    phrase.date = as.Date(phrase.datum, format = "%Y%m%d")

    bdnames = phrase.date %>%
        as.character() %>%
        stringr::str_replace_all("-", ".")

    # prepend the prefix to date information
    bdnames = map_chr(bdnames, function(x) paste0(prefix, ".", x))

    if (writeout == TRUE){
        outfile = paste0(file, ".txt")
        write.csv(bdnames, file = outfile, row.names = FALSE)
    } else return(bdnames)
}

# DEMO
# bandnames("F:/geodata/geo402/S2/xx_S2_indices/ladybrand35JMH/stack_35JMH_ndvi.img", "ndvi")


################################################################################
# Rename bandnames -------------------------------------------------------------
################################################################################

rename_bandnames = function(rasterfile, textfile = NULL){

    if (is.null(textfile)){stop("Define a textfile contain bandnames of all layers in the raster.\n")}

    # load raster
    ras = brick(rasterfile)

    # read in csv data
    naming = read.csv(file = textfile, colClasses = "character") %>%
        .$x

    date = naming %>%
        stringr::str_split(string = ., pattern = "\\.") %>%
        map(., ~ .x[2:length(.x)]) %>%
        map_chr(., ~ stringr::str_flatten(.x)) %>%
        as.Date(., format = "%Y%m%d")

    if (!nlayers(ras) == length(date)){stop("The raster and the textfile do not have the same length and can thus not be combined\n")}

    # final tranfer: Set Z-Dimension to raster
    ras = setZ(ras, date, name = "date")
    names(ras) = naming

    return(ras)
}


################################################################################
# exactextracting, fully replaces gt_from_raster: faster and more stable!
################################################################################

# can take NA rasters!

exactextracting = function(gt, ras, col_class, col_id, statistics, dstdir, outfile){
    #' 1st order list: classes
    #' 2nd order list: raster bands
    #' 3rd order dataframe: aggregated statistics with smoothing curves

    library(exactextractr)

    if (!is_vector(gt[[col_id]])){
        stop("the col_id does not exist, please specify...")
    }
    if ((!is_vector(gt[[col_class]]))){
        stop("the col_class does not exist, please specify...")
    }

    layernames = names(ras)

    # create dirs
    if (!dir.exists(dstdir)) {dir.create(dstdir)}

    # extracting
    print("Feature Extraction starting . . .")
    all_data = exactextractr::exact_extract(ras, gt, statistics) # calculate means

    # join classes on extracted data for tidying pipe coming
    join = mutate(all_data, class = gt[[col_class]], id = gt[[col_id]])
    join[is.na(join)] = NA

    outer = list()

    # outer = vector("list", length = length(unique(join$class)))
    # iterate by class
    for (i in sort(unique(join$class))){
        inner = list()

        cat("Calculating Class No.:", i, "\n")
        ij = filter(join, join$class == i)

        # iterate by number
        for (j in sort(unique(ij$id))){

            # print(j)
            # wrangle dataframe
            entity = filter(join, join$class == i & join$id == j) %>%
                dplyr::select(-c(class, id)) %>%
                t() %>%
                as.data.frame() %>%
                mutate(rowname = row.names(.))

            date_raw = entity$rowname

            # get date
            date = substr(date_raw, start = nchar(date_raw) - 9, stop = nchar(date_raw))
            date = as.POSIXct(date, tryFormats = "%Y.%m.%d") %>%
                unique() %>%
                as.data.frame() %>%
                `colnames<-`("date")

            # init single dataframe
            stat = list()
            for (h in seq_along(statistics)){
                metric = statistics[h]
                data = entity %>% filter(str_detect(rowname, metric)) %>%
                    dplyr::select(-rowname) %>%
                    `colnames<-`(metric)
                stat[[h]] = data
            }

            # bind lists to dataframe
            stat = cbind(stat, date)

            if (nrow(stat) == length(stat$med[stat$med == TRUE])) next
            # print(j)

            # smoothing functions
            smooth_names = c("med_smooth", "losd_smooth", "upsd_smooth")

            # functions
            med_smooth = supsmu(stat$date, stat$med)
            losd_smooth = supsmu(stat$date, stat$mean - stat$stdev)
            sd_smooth = supsmu(stat$date, stat$mean + stat$stdev)

            # listed
            smooth = list(med_smooth, losd_smooth, sd_smooth)

            single_columns = map2(smooth, smooth_names, function(x, y){
                df = data.frame(x$x, x$y) %>% `names<-`(c("date", y))
            })

            stat_smoothed = left_join(stat, single_columns, by = "date", copy = TRUE, keep = FALSE) %>%
                dplyr::select(-c(date.1, date.2))

            # create list and rename ij table
            entity = list(as.data.frame(stat_smoothed)) %>% `names<-`(j)
            inner = append(inner, entity)
        }

        # iteratively add the lists of dataframes of the classes to a master (outer) class
        outer[[i]] = inner

    }
    saveRDS(outer, paste0(dstdir, "/", outfile))
    return(outer)
}


################################################################################
# extracting summary statistics for all classes!
################################################################################

extract_summary = function(gt, ras, col_class){

    #' default statistics implemented are:
    #' Median
    #' Mean
    #' Standard Deviation
    #' smoothed median
    #' smoothed standard deviations (upper and lower)

    # pre ----------------------------------------------------------------------
    layernames = names(ras)
    medianname = paste("med", layernames, sep = ".")

    raslist = list()
    for (i in 1:nlayers(ras)){
        r = ras[[i]]
        raslist = append(raslist, r)
    }

    nr_classes = length(unique(gt[[col_class]]))

    # console output:

    cat("Number of raster layers:", nlayers(ras), "\n")
    cat("Number of classes:", nr_classes, "\n\n\n")


    # date -------------------------------------------------------------------------
    date = substr(layernames, start = nchar(layernames) - 9, stop = nchar(layernames))
    date = as.POSIXct(date, tryFormats = "%Y.%m.%d") %>%
        unique()

    # data processing ----------------------------------------------------------
    cat("processing median\n\n")
    ex = map(raslist, ~ exact_extract(.x, gt, "mean")) %>%
        as.data.frame(col.names = medianname) %>%
        mutate(class = gt[[col_class]])

    # init class list
    summ = vector("list", length = nr_classes)
    names = c()

    # sorting by class
    for (i in 1:nr_classes){
        # print(i)
        cls = sort(unique(gt[[col_class]]))[[i]]
        summ[[i]] = filter(ex, gt[[col_class]] == cls) %>% dplyr::select(-class)
        names = c(names, cls)
        }

    # assign class names
    names(summ) = names

    # wrangling: calculating metrics from the median ---------------------------
    cat("\n\nprocessing other metrics\n")

    # function definition:
    s.median = as.function(x = alist(a = , median(a, na.rm = TRUE)), envir = globalenv())
    s.mean = as.function(x = alist(a = , mean(a, na.rm = TRUE)), envir = globalenv())
    s.sd = as.function(x = alist(a = , sd(a, na.rm = TRUE)), envir = globalenv())

    s.list = list(s.median, s.mean, s.sd)
    s.colnames = c("median", "mean", "sd")

    summ2 = map(summ, function(x){
        stat = list()
        for (h in seq_along(s.list)){

            data = x %>%
                # na.omit() %>%
                summarise_all(.funs = s.list[h]) %>%
                t() %>%
                `colnames<-`(s.colnames[h])

            #' count the number of reference site accounted for the statistics
            #' many are lost by missing data, especially Sentinel-2 probes
            NAs = x %>%
                summarise_all(~(sum(!is.na(.)))) %>%
                t() %>%
                as.data.frame() %>%
                `colnames<-`("count")

            stat = cbind(stat, data) %>%
                as.data.frame() %>%
                `row.names<-`(NULL)
        }

        # add date
        stat = stat %>% mutate(NAs, date)

        stat$median = as.numeric(stat$median)
        stat[is.na(stat)] = NA
        return(stat)
    })

    # add smoothing functions --------------------------------------------------
    # smoothing functions
    smooth_names = c("med_smooth", "losd_smooth", "upsd_smooth")

    summ3 = map(summ2, function(x){

        # process the smoothing
        med_smooth = supsmu(x$date, x$median)
        losd_smooth = supsmu(x$date, x$mean - x$sd)
        sd_smooth = supsmu(x$date, x$mean + x$sd)

        # list the results
        smooth = list(med_smooth, losd_smooth, sd_smooth)

        # create proper columns from it
        single_columns = map2(smooth, smooth_names, function(x, y){
            df = data.frame(x$x, x$y) %>% `names<-`(c("date", y))
        })

        # join commands to the master data.frame `x`
        all_columns = reduce(single_columns, left_join, by = "date")
        joined = left_join(x, all_columns, by = "date")
        return(joined)
    })

    cat("finished")
    return(summ3)
}

################################################################################
# Coercing large rasters to data.tables ----------------------------------------
################################################################################

#' Transform raster to data.table
#'
#' @param x  Raster* object
#' @param row.names	`NULL` or a character vector giving the row names for the data frame. Missing values are not allowed
#' @param optional	logical. If `TRUE`, setting row names and converting column names (to syntactic names: see make.names) is optional
#' @param xy  logical. If `TRUE`, also return the spatial coordinates
#' @param centroids	logical. If TRUE return the centroids instead of all spatial coordinates (only relevant if xy=TRUE)
#' @param sepNA	logical. If TRUE the parts of the spatial objects are separated by lines that are NA (only if xy=TRUE and, for polygons, if centroids=FALSE
#' @param ...	 Additional arguments (none) passed to `raster::as.data.frame`
#'
#' @value returns a data.table object
#' @examples
#' logo <- brick(system.file("external/rlogo.grd", package="raster"))
#' v <- as.data.table(logo)
#' @import
#'
#' credits: https://gist.github.com/etiennebr/9515738, etiennebr/as.data.table.r

as.data.table.raster <- function(x, row.names = NULL, optional = FALSE, xy=FALSE, inmem = canProcessInMemory(x, 2), ...) {
    stopifnot(require("data.table"))
    if(inmem) {
        v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
        coln <- names(x)
        if(xy) coln <- c("x", "y", coln)
        setnames(v, coln)
    } else {
        tr <- blockSize(x)
        l <- lapply(1:tr$n, function(i) {
            DT <- as.data.table(as.data.frame(getValues(x, row = tr$row[i], nrows = tr$nrows[i]), ...))
            if(xy == TRUE) {
                cells <- cellFromRowCol(x, c(tr$row[i], tr$row[i] + tr$nrows[i] - 1), c(1, ncol(x)))
                coords <- xyFromCell(x, cell = cells[1]:cells[2])
                DT[, c("x", "y") := data.frame(xyFromCell(x, cell = cells[1]:cells[2]))]
            }
            DT
        })
        v <- rbindlist(l)
        coln <- names(x)
        if(xy) {
            coln <- c("x", "y", coln)
            setcolorder(v, coln)
        }
    }
}



################################################################################
# binds together three input data.frames with coordinates and class column "class"
################################################################################

bind_task = function(list){

    #' @param list: list of dataframes containing pixels(observations) in rows and date in columns.
    #' Requires a form from gt_from_raster call
    #'
    #' this function works only in conjuction to gt_from_raster. More functionality to be developed.
    #' get data in tibble (open functionabliy to check duplicate column names)

    input = bind_cols(list) %>%
        as_tibble(.name_repair = "unique")

    # remove cols with x, y and class from the data frame,
    # rename vars from the last binded data frame to x, y and class
    out = input %>%
        .[,1:(length(.) - 2)] %>%
        dplyr::select(-ends_with("x"), -ends_with("y"), -contains("class")) %>%
        cbind(input[,(length(input) - 2):length(input)]) %>%
        dplyr::rename(x = ends_with("x"),
                      y = ends_with("y"),
                      class = contains("class")) %>%
        as.data.frame()

    # warnings
    if (sum(is.na(out)) != 0L){warning("please remove all NA from the input layers")}
    if (!class(out$class) == "factor"){
        warning("class needs to be a factor!")
    }

    # identify count of NAs in data frame
    sum(is.na(out))
    names(out)

    cat("number of variables:", "\n")
    cat(length(names(out)))

    return(out)

    # remove cols with NA (prerequisit for random forest input)
    # dt = dt2 %>%
    #     as.data.frame() %>%
    #     .[, colSums(is.na(.)) == 0] %>%
    #     as.data.table()
    # watch NA
    # is.na(dt2)
    # which(is.na(dt2))
    # tibble::enframe(names(out)) %>% count(value) %>% filter(n > 1) # check if columns are duplicates!
}

################################################################################
# similar to bind_task but can coerce newdata tasks with data.tables
################################################################################

bind_newdata = function(list){

    # save coords
    coords = list[[1]] %>%
        dplyr::select(ends_with("x") | ends_with("y"))

    out = bind_cols(list) %>%
        dplyr::select(-contains("x"), -contains("y"), -contains("class")) %>%
        mutate(., x = coords$x, y = coords$y) # add them again

    # warnings
    if (!is.numeric(out$y)){
        warning("add a coordinate column!")
    }

    # identify count of NAs in data frame
    sum(is.na(out))

    cat("number of variables:", "\n")
    cat(length(names(out)))

    return(out)
}

################################################################################
# warps rasters to chunks which are easier to process
################################################################################

warp_tiles = function(raster, extent, outname){

    # warp to smaller extent (4 tiles in total)
    gdalUtils::gdalbuildvrt(gdalfile = raster,
                            output.vrt = path_vrt,
                            overwrite = TRUE,
                            te = extent,
                            a_srs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

    gdalUtils::gdal_translate(src_dataset = path_vrt,
                              dst_dataset = paste0(path_s2, outname, ".tif"),
                              overwrite = TRUE)
}

################################################################################
# for export of classification product
################################################################################

exporting = function(output, input, filepath){

    # bind coords on data.table
    out5 = cbind(output, x = input$x, y = input$y)

    # make sf coords
    out4 = st_as_sf(out5, coords = c("x", "y"))

    # set crs
    st_crs(out4) = 32735

    # to sp for gridding, functionality is not yet found in sf... st_rasterize may work in `stars`
    out3 = as(out4, "Spatial")

    # gridding
    gridded(out3) = TRUE
    class(out3)

    outfile = stack(out3) %>%
        trim()

    writeRaster(outfile, filename = paste0(filepath, ".tif"),
                format="GTiff", datatype='FLT4S', overwrite=TRUE, na.rm=TRUE)
}

################################################################################
# easy making chunks a dataframe and store on disk
################################################################################

save_chunk_to_dataframe = function(x, outfile, option, var_prefix, naming){

    brick(x) %>%
    rename_bandnames(option, var_prefix, naming) %>%
    as.data.table.raster(xy = TRUE, inmem = FALSE) %>%
    write_rds(path = outfile)

}

################################################################################
# Wrapper for classifiction (making model every time)
################################################################################

# deprecated
classif_default = function(newdata, outfile){

    # make task
    task_slangbos = TaskClassifST$new(id = "slangbos", backend = input, target = "class",
                                      coordinate_names = c("x", "y"),
                                      crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs")

    # define learner
    learner = lrn("classif.ranger", predict_type = "prob")

    # set built-in filter & hyperparameters
    learner$param_set$values = list(num.trees =375L, mtry = 2L)

    # multicore application
    future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

    # train
    learner$train(task_slangbos)

    # prediction
    pred = learner$predict_newdata(task = task_slangbos, newdata = newdata)

    output = data.table::as.data.table(pred)
    exporting(output = output, input = newdata, filepath = paste0(path_prediction, outfile))

}

################################################################################
# Wrapper for classifiction (loading model)
################################################################################

classif = function(newdata, path_model, outfile){

    # load trained model:
    learner = readRDS(path_model)

    cat("Model: ", sep = "\n")
    print(learner)
    future::plan("multiprocess") # anmerkung: future::plan turns over order of logging in the resample algorithm! -> Patrick S sagen

    # prediction
    cat("predicting . . .", sep = "\n")
    pred = learner$predict_newdata(newdata = newdata)

    output = data.table::as.data.table(pred)
    exporting(output = output, input = newdata, filepath = paste0(path_prediction, outfile))

}

################################################################################
# Beautiful theme
################################################################################

## map themes for ggplot (https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/)
theme_map <- function(...) {
    theme_minimal() +
        theme(
            text = element_text(family = "Ubuntu Regular", color = "#22211d"),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.background = element_rect(fill = "#f5f5f2", color = NA),
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.border = element_blank(),
            ...
        )
}

################################################################################
# Get data of one gt element
################################################################################

grep2 = function(source, class, number = NULL){

    # if there is no number given -> return the whole class
    if (is.null(number)){

        cls = as.character(class)
        table = source[[cls]] # indexing task

        # cat("Count of list items (Class instances):", length(table))
        # cat("Ground truth set: ", deparse(substitute(source)), cls, sep = "\n")

        for (n in 1:length(table)){
            str = table[[n]]$date

            table[[n]]$date = as.POSIXct(substr(table[[n]]$date, start = nchar(table[[n]]$date) -9,
                                                stop = nchar(table[[n]]$date)),
                                         format = "%Y.%m.%d", tz = "")
        }


    } else {

        # if there is a number given -> return just one

        cls = as.character(class) # convert to string (list headers are strings not int)
        num = as.character(number) # same here
        # cat("Ground truth set: ", deparse(substitute(source)), cls, num, sep = "\n") # tidy print
        table = source[[cls]][[num]] # indexing task

        # convert date to POSTix
        table$date = as.POSIXct(substr(table$date, start = nchar(table$date) -9, stop = nchar(table$date)),
                                format = "%Y.%m.%d", tz = "")
    }

    if (is.null(table)){stop("This number doesn't exist")}

    return(table)
}

################################################################################
# filter position out
################################################################################

position = function(gt, class_nr, sample_nr){
    #' class must be number
    #' based on tidy sf "gt"
    filter(gt, class_simple == class_nr & id == sample_nr)
}

################################################################################
# scale data from gt
################################################################################

scale_custom = function(x, fun = NULL, center = FALSE){
    m = as.matrix(x[2:6])
    m = scale(m, center=center)

    # # main scale function
    # if (is.null(fun)){m = scale(m, center=center)
    # } else if (!is.null(fun)){m = scale(m, center=center}

    out = as.data.frame(m) %>%
        mutate(date = x$date, count = x$count)
    cat("data normalised!")
    return(out)
} # scaling the parameters

#, scale=colSums(m)

################################################################################
# Extracting stats of the ground truth elements and applies smoothing
################################################################################

# buggy
stats = function(dfs){

    out = list()
    for (c in seq_along(dfs)){
        data = dfs[[c]]
        data = map(data, function(x) na.omit(x))
        map(data, function(x) mutate(x, med_smooth = supsmu(x$date, x$med)$y,
                                     losd_smooth = supsmu(x$date, x$mean - x$stdev)$y,
                                     upsd_smooth = supsmu(x$date, x$mean + x$stdev)$y))
        out = append(out, data)
    }
    return(data)
}
