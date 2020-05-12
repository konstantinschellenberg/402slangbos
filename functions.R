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
# Make mask from QA bands ------------------------------------------------------
################################################################################

make_mask = function(x){
    out = x == 2 | x == 3; return(out)
}

################################################################################
# RVI definition ---------------------------------------------------------------
################################################################################

rvi = function(vv, vh){
    4 * vh / (vv + vh)
}

################################################################################
# NDVI definition ---------------------------------------------------------------
################################################################################

fun.ndvi = function(r, n){(n-r)/(n+r)}

################################################################################
# Rename bandnames -------------------------------------------------------------
################################################################################

rename_bandnames = function(raster = NULL, var_prefix = NULL, naming = NULL){

    #' param. raster to be renames
    #' var_prefix. prefix of the raster bands
    #' naming. table.txt with layer names in rows

    # read in csv data
    date_in_bandnames = read.csv(file = naming, colClasses = "character") %>%
        .$x

    # convert date string into R date-time format
    date = c()
    for (i in 1:length(date_in_bandnames)){
        date = append(date, as.POSIXct(date_in_bandnames[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }

    a = length(names(raster)) - length(date)

    if (a != 0){warning("Number of raster bands does not fit the input table")}

    # final tranfer
    names(raster) = paste0(var_prefix, ".", date) # change names to more easy
    return(raster)
}

################################################################################
# gt_from_raster----------------------------------------------------------------
################################################################################

gt_from_raster = function(train_data = NULL,
                          response_col = NULL,
                          raster = NULL,
                          dst = NULL,
                          prefix = NULL){

    # credits to http://amsantac.co/blog/en/2015/11/28/classification-r.html
    # https://gist.github.com/amsantac/5183c0c71a8dcbc27a4f
    outest = list()
    cat("load raster as velox-object... please wait.")
    v = velox(raster)

    for (i in 1:length(unique(train_data[[response_col]]))){

        # get class
        category = unique(train_data[[response_col]])[i]
        cat("Category:", category, "\n")

        # returns sp polygon with class i
        categorymap = train_data[train_data[[response_col]] == category,]

        # extract pixel information
        # dataSet = raster::extract(raster, categorymap, cellnumbers = TRUE) ## deprecated
        dataSet = v$extract(sp = categorymap)

        out = list()

        for (a in dataSet){

            # replace colnames of matrix by raster's colnames (preferrably dates)
            colnames(a) = names(raster)

            calcs = a %>%
                t() %>%
                as.data.frame() %>%
                mutate(date = colnames(a)) %>%
                pivot_longer(-date, names_to = "names", values_to = "values") %>%
                group_by(date) %>%
                summarise(mean = mean(values), # here can be put more stats information retrieved from the polygons
                          median = median(values),
                          sd = sd(values),
                          "lower_sd" = mean(values) - sd(values),
                          "upper_sd" = mean(values) + sd(values),
                          count = n())

             out = append(out, list(as.data.frame(calcs)))
        }

        # rename inner lists (of the categories)
        names(out) = c(seq(1:nrow(categorymap)))

        # append to master-list (outest)
        outest = append(outest, list(out))
    }

    names(outest) = unique(train_data[[response_col]])
    saveRDS(outest, paste0(dst, prefix, "_gt_list.rds"))
}

################################################################################
# Learning table ---------------------------------------------------------------
################################################################################

# not working!!!!!!

learning_table = function(train_data = NULL,
                          response_col = NULL,
                          raster = NULL,
                          dst = NULL,
                          prefix = NULL){

    xname = paste0(prefix, "_x")
    yname = paste0(prefix, "_y")

    # credits to http://amsantac.co/blog/en/2015/11/28/classification-r.html
    # https://gist.github.com/amsantac/5183c0c71a8dcbc27a4f7

    df_all = data.frame(matrix(vector(), nrow = 0, ncol = length(names(raster)) + 1))

    for (i in 1:length(unique(train_data[[response_col]]))){

        # get class
        category = unique(train_data[[response_col]])[i]
        cat("Category:", category, "\n")

        # returns sp polygon with class i
        categorymap = train_data[train_data[[response_col]] == category,]

        # extract pixel information
        dataSet = raster::extract(raster, categorymap, cellnumbers = TRUE) ## deprecated
        # dataSet = v$extract(sp = categorymap)

        out = list()

        # making dataset for machine learning-----------------------------------
        dataSet2 = NULL

        # writing coordinates to the matrix of each gt element
        for (j in seq_along(dataSet)){
            table = dataSet[[j]]
            coords = coordinates(raster)[table[,1],] # getting coordinates from raster cell number
            colnames(coords) = c(xname, yname)
            coords_binded = cbind(table, coords) # bind them to extract output

            dataSet2 = append(dataSet2, list(coords_binded)) # make list output
        }

        dataSet3 = dataSet2[!unlist(lapply(dataSet2, is.null))] %>%
            lapply(function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})

        df = do.call("rbind", dataSet3)
        df_all = rbind(df_all, df)
    }

    df_all = df_all[, -1]
    df_all$class = as_factor(df_all$class)
    saveRDS(df_all, paste0(dst, prefix, "_learning_input.rds"))
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

# if (!isGeneric("as.data.table")) {
#     setGeneric("as.data.table", function(x, ...)
#         standardGeneric("as.data.table"))
# }
#
# setMethod('as.data.table', signature(x='data.frame'), data.table::as.data.table)
# setMethod('as.data.table', signature(x='Raster'), as.data.table.raster)

################################################################################
# remove cloud layers from brick within the given fraction ---------------------
################################################################################

# clouds have to be masked already and have NA values.

remove_cloud_layers = function(x, outfile, fraction = 0.2){

    x.clouds = cellStats(is.na(x), sum) # count class 1 (clouds)

    x.clouds.fraction = x.clouds/ncell(x) # fraction that is NA
    x.clouds.fraction

    cat(paste("fraction of clouds in images: ", sep = "\n"))
    cat(sort(round(x.clouds.fraction, digits = 2), decreasing = TRUE), sep = "\n")

    x.keep = x[[which(x.clouds.fraction < fraction)]] # filter all with clouds less than the given faction of cloud cover

    cat(paste("Layers kept: ", sep = "\n"))
    cat(names(x.keep))
    cat("layers deleted:")
    cat(length(names(x.clouds)) - length(names(x.keep)), sep = "\n")

    cat("Writing raster, please be patient . . . ")
    writeRaster(x.keep, filename = outfile, overwrite = TRUE)
    x.keep = brick(outfile)

    return(x.keep)
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
        select(ends_with("x") | ends_with("y"))

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

position = function(class, number = Null){
    gt %>%
        filter(Name == class) %>%
        .[number, ] %>%
        st_transform(4326)
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

stats = function(list.dataframes, c, n, coherence_smoothing = FALSE){
    #' function to warp data of a ground truth segment of choice in readable lists
    #' digestable for plotting
    #' "all" within the list's name indicated exclusion from the smoothing calculation
    #' due to NA values in the table

    # get names of the dataframes
    names = list.names(list.dataframes)

    # initialise list
    list = list()
    for(i in list.dataframes){
        new = list(grep2(i, c, n))
        list = append(list, new)
    }

    # set names of the new dataframes list
    names(list) = names

    # add median smoothed column
    if (coherence_smoothing == FALSE){
        list2 = list[!grepl(pattern = "all", x = names(list))]
        all = list[grepl(pattern = "all", x = names(list))]

        list2 = lapply(list2, function (x) drop_na(x))
        list2 = lapply(list2, function (x) mutate(x, med_smooth = supsmu(x$date, x$median)$y))
        list2 = lapply(list2, function (x) mutate(x, losd_smooth = supsmu(x$date, x$lower_sd)$y))
        list2 = lapply(list2, function (x) mutate(x, upsd_smooth = supsmu(x$date, x$upper_sd)$y))
        names(list2)
        names(all)
        list = append(list2, all)
        names(list)
    } else {
        list = lapply(list, function (x) mutate(x, med_smooth = supsmu(x$date, x$median)$y))
        list = lapply(list, function (x) mutate(x, losd_smooth = supsmu(x$date, x$lower_sd)$y))
        list = lapply(list, function (x) mutate(x, upsd_smooth = supsmu(x$date, x$upper_sd)$y))
    }

    # calc NDVI
    ndvi.pre.red = grep2(gt_red, c, n)
    ndvi.pre.nir = grep2(gt_nir, c, n)

    ndvi = data.frame(matrix(nrow = nrow(list$nir), ncol = 2))
    ndvi$X2 = fun.ndvi(list$red$median, list$nir$median)
    ndvi$X1 = list$red$date
    names(ndvi) = c("date", "ndvi")

    # optional scaling of the ndvi (only for comparison purposes, scale on stdev)
    # ndvi$X2 = scale(ndvi$X2, center = T) %>% as.vector()

    list = list.append(list, ndvi = ndvi)
    return(list)
}

################################################################################
# Checks NA statistics on a Raster Stack
################################################################################

na_identifier = function(stack, na){
    if (is.na(na)) {stack.na = cellStats(is.na(stack), sum)}
    else {stack.na = cellStats(stack == na, sum)} #count the NA (-99) values in each layer
    stack.na.fraction = stack.na/ncell(stack) # fraction that is NA (-99)
    filter_less20 = stack[[which(stack.na.fraction < 0.2)]] # filter all with NA less 20% coverage
    filter_less10 = stack[[which(stack.na.fraction < 0.1)]] # filter all with NA less 10% coverage
    filter_0 = stack[[which(stack.na.fraction == 0)]] # filter all with 0% NA coverage

    # cat("Layer names where less than 20% is covered with NAs\n")
    cat("20%: Number of layers", nlayers(filter_less20), "- Fraction of total layers", nlayers(filter_less20)/nlayers(stack)*100, "%", sep = " ")
    cat("\n")

    # cat("Layer names where less than 10% is covered with NAs\n")
    cat("10%: Number of layers", nlayers(filter_less10), "- Fraction of total layers", nlayers(filter_less10)/nlayers(stack)*100, "%", sep = " ")
    cat("\n")

    # cat("Layer names where 0% is covered with NAs\n")
    cat("0%: Number of layers", nlayers(filter_0), "- Fraction of total layers", nlayers(filter_0)/nlayers(stack)*100, "%", sep = " ")
    cat("\n")

    cat("Mean NA values in scenes:", mean(stack.na))

}

