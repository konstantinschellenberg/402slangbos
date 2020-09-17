
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
        dataSet = raster::extract(raster, categorymap, cellnumbers = TRUE)
        # dataSet = v$extract(sp = categorymap) # deprecated under R 4.x

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
