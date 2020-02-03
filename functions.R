# Konstantin Schellenberg, WS 2019/20
# University of Jena, Chair of remote sensing
# supervisor: Dr. Marcel Urban

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
#     1 == slangbos encroachment, increase
#     12 == encroachment with cleaning/breakpoint
#     2 == cleaning/breakpoint
#     3 == continous persistance of slangbos
#     4 == agricultural site

# load dependency

################################################################################
# Make mask from QA bands ------------------------------------------------------
################################################################################

make_mask = function(x){out = x == 2 | x == 3; return(out)}

################################################################################
# Rename bandnames -------------------------------------------------------------
################################################################################

rename_bandnames = function(raster = s1vv){

    bandnames = names(raster)

    # iterate for date in column-names
    for (i in bandnames){
        date_in_bandnames = substr(bandnames,13,20)
    }

    # convert date string into R date-time format
    date = c()
    for (i in 1:length(date_in_bandnames)){
        date = append(date, as.POSIXct(date_in_bandnames[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }
    dates_raster <<- date # write as global variable
    names(raster) = paste0(date) # change names to more easy
    return(raster)
}

################################################################################
# gt_from_raster----------------------------------------------------------------
################################################################################

gt_from_raster = function(train_data = gt,
                          response_col = "Name",
                          raster = raster_test,
                          outfile = "test"){

    # credits to http://amsantac.co/blog/en/2015/11/28/classification-r.html
    # https://gist.github.com/amsantac/5183c0c71a8dcbc27a4f
    df_all = data.frame(matrix(vector(), nrow = 0, ncol = length(names(raster)) + 1))
    outest = list()

    for (i in 1:length(unique(train_data[[response_col]]))){

        # get class
        category = unique(train_data[[response_col]])[i]
        print(category)
        # returns sp polygon with class i
        categorymap = train_data[train_data[[response_col]] == category,]
        plot(categorymap[0])

        # extract pixel information
        dataSet = raster::extract(raster, categorymap, cellnumbers = TRUE)

        out = list()

        for (a in 1:length(dataSet)){

            print("inner", a)
            remove_cell = as.data.frame(dataSet[[a]]) %>%
                .[, -1] # this removes the first column "cell" from cellnumbers

            calcs = remove_cell %>%
                t() %>%
                as.data.frame() %>%
                mutate(date = colnames(remove_cell)) %>%
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

        # making dataset for machine learning-----------------------------------
        dataSet2 = NULL

        for (i in dataSet){ # writing coordinates to the matrix of each gt element
            coords = coordinates(raster)[i[,1],] # getting coordinates from raster cell number
            coords_binded = cbind(i, coords) # bind them to extract output
            dataSet2 = append(dataSet2, list(coords_binded)) # make list output
        }

        dataSet3 = dataSet2[!unlist(lapply(dataSet2, is.null))] %>%
            lapply(function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})

        df = do.call("rbind", dataSet3)
        df_all = rbind(df_all, df)

        # ----------------------------------------------------------------------

        # rename inner lists (of the categories)
        names(out) = c(seq(1:nrow(categorymap)))

        # append to master-list (outest)
        outest = append(outest, list(out))
    }
    df_all = df_all[, -1]
    df_all$class = as_factor(df_all$class)
    names(outest) = unique(train_data[[response_col]])
    saveRDS(df_all, paste0(rds_path, "learning_input_", outfile, ".rds"))
    saveRDS(outest, paste0(rds_path, "gt_list_", outfile, ".rds"))
}


################################################################################
# Function definition `carve_brick`---------------------------------------------
################################################################################

carve_brick = function(sentinel1_brick,
                        polygon,
                        code = 1,
                        gt_example_no = 1){

    # built-in function for naming gts
    namer <<- function(code){

    codename = ""

    # looping for plotting name
    if (code == 1) {
        codename = "Increase"
    }
    if (code == 2) {
        codename = "Cleaned"
    }
    if (code == 12) {
        codename = "Increase, then cleaned"
    }
    if (code == 3) {
        codename = "Continuous"
    }
    if (code == 4) {
        codename = "Agriculture"
    }
    return(codename)
    }

    # filtering code
    gt_code = filter(polygon, polygon$Name == code)

    # indexing single object
    single_gt = gt_code[gt_example_no, 1]

    # spatial subset with single gt bounds
    subset = raster::extract(sentinel1_brick, single_gt) %>%
        as.data.frame()

    # convert band names to date
    bandnames = names(subset)

    # iterate for date in column-names
    for (i in bandnames){
        date = substr(bandnames,13,20)
    }

    # convert date string into R date-time format
    date_s1 = c()
    for (i in 1:length(date)){
        date_s1 <- append(date_s1, as.POSIXct(date[i], format = "%Y%m%d")) #https://www.statmethods.net/input/dates.html
    }
    ####

    datenames <<- date_s1

    # integrate date to dataset: making time series
    # calculating the mean and margins (stdev), one transposition t() needed here
    df_date = subset %>%
        t() %>%
        as.data.frame() %>%
        mutate(date = date_s1) #%>%
        # na.omit()

    df = pivot_longer(df_date,
                      -date,
                      names_to = "names",
                      values_to = "values"
    )

    df_summary = df %>%
        group_by(date) %>%
        summarise(mean = mean(values),
                  median = median(values),
                  sd = sd(values),
                  "lower_sd" = mean(values) - sd(values),
                  "upper_sd" = mean(values) + sd(values),
                  count = n())

    # printing summary to console
    paste = paste(
        paste("Size of the plot:", st_area(single_gt[1,]), sep = " "),
        paste("count of pixels in the timestack:", nrow(df), sep = " "),
        paste("count of pixels in the polygon:", median(df_summary$count), sep = " "),
        paste("gt of type: ", namer(code), sep = " "),
        paste("median = ", mean(df_summary$median, na.rm = TRUE), sep = " "),
        paste("mean = ", mean(df_summary$mean, na.rm = TRUE), sep = " "),
        paste("standard deviation = ", mean(df_summary$sd, na.rm = TRUE), "\n", sep = " "),
        sep = "\n")
    ####

    cat(paste)
    return(df_summary)
}

################################################################################
# Developing loop for `carve_brick`---------------------------------------------
################################################################################


list_summaries = function(sentinel1_brick, polygon){

    # help for data.frame in loops:
    # https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
    # set up function to set back all counters and variables before running the loops below
    initialise_counters = function(){
        code <<- 1
        ct_1 <<- 1
        ct_12 <<- 1
        ct_2 <<- 1
        ct_3 <<- 1
        ct_4 <<- 1
    }

    # run the intialisation
    initialise_counters()
    summary = list()
    counter = 1

    # for loop to write dataframes for each gt to the summary list
    for (code in gt$Name) {

        print(counter)

        if (code == 1) { #increase
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, gt_example_no = ct_1))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_1)
            ct_1 = ct_1 + 1

        } else if (code == 12) { #increase, then cleaned
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, gt_example_no = ct_12))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_12)
            ct_12 = ct_12 + 1

        } else if (code == 2) { #cleaning
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, gt_example_no = ct_2))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_2)
            ct_2 = ct_2 + 1

        } else if (code == 3) { #continuous
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, gt_example_no = ct_3))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_3)
            ct_3 = ct_3 + 1

        } else if (code == 4) { #agriculture
            new = list(carve_brick(sentinel1_brick = sentinel1_brick,
                                   polygon = polygon,
                                   code = code, gt_example_no = ct_4))
            summary = append(summary, new)
            names(summary)[length(summary)] = paste0("plot", code, "_", ct_4)
            ct_4 = ct_4 + 1

        } else {
            print("Not correctly assigned gt code")
        }
        counter = counter + 1
    }

    statement = paste(
        paste("Number of code 1:", ct_1),
        paste("Number of code 12:", ct_12),
        paste("Number of code 2:", ct_2),
        paste("Number of code 3:", ct_3),
        paste("Number of code 4:", ct_4),
        sep = "\n")

    # print statement to console
    cat(statement)

    return(summary)
}
