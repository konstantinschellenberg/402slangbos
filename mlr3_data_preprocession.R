# Script for pre-procession and harmonisation of S2 and S1 data
# Konstantin Schellenberg

source("import.R")

######################################################################
# Training Dataset
######################################################################

# create input tables

# full
x = 0
if (x == 1){

    # remove files
    file.remove(c(paste0(path_rds, "learning_input_vh.rds"),
                  paste0(path_rds, "learning_input_red.rds"),
                  paste0(path_rds, "learning_input_nir.rds")))

    # make new ground truth tables
    gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
    gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
    gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")
} else {

    # read in
    vh_input = readRDS(paste0(path_rds, "learning_input_vh.rds"))
    red_input = readRDS(paste0(path_rds, "learning_input_red.rds"))
    nir_input = readRDS(paste0(path_rds, "learning_input_nir.rds"))

}



# merge data.frames, find out with cols are dublicates, coords and class column only once
input = bind_task(vh_input, red_input, nir_input)

# write to disk
write_rds(input, path = paste0(path_developement, "rda/input.rds"))

# read in
input = readRDS(paste0(path_developement, "rda/input.rds"))


######################################################################
# Prediction Dataset chunk-wise
######################################################################

name = "vh_rightbottom"
var_prefix = "vh"

# transfers raster to datatable and stores it on disk
# full read-in provided via rename_bandnames

save_chunk_to_dataframe(x = paste0(path_s2, name, ".tif"),
                        outfile = paste0(path_rds, name, ".rds"),
                        option = 1,
                        var_prefix = var_prefix,
                        naming = path_naming_s1)

newdata1 = readRDS(paste0(path_rds, "vh_rightbottom.rds"))
newdata2 = readRDS(paste0(path_rds, "red_rightbottom.rds"))
newdata3 = readRDS(paste0(path_rds, "nir_rightbottom.rds"))

newdata = as.data.table(bind_newdata(newdata1, newdata2, newdata3))
newdata[is.na(newdata)] = -99

write_rds(newdata, path = paste0(path_rds, "newdata_rightbottom.rds"))

######################################################################
# subsetting input tables (optional)
######################################################################

newdata = readRDS(paste0(path_rds, "newdata_rightbottom.rds")) %>%
    as.data.table()

class(newdata)

begin = 1
half = length(rownames(newdata))/2
end = length(rownames(newdata))

newdata.split1 = newdata[half:end,]
newdata.split2 = newdata[begin:half,]

# optional writing to disk
c = 1
if (c == 1){
    write_rds(newdata.split1, path = paste0(path_developement, "rda/newdata_split1_datatable.rds"))
    write_rds(newdata.split2, path = paste0(path_developement, "rda/newdata_split2_datatable.rds"))

    ### read
    newdata.split1 = readRDS(paste0(path_developement, "rda/newdata_split1_datatable.rds"))
    newdata.split2 = readRDS(paste0(path_developement, "rda/newdata_split2_datatable.rds"))
}

# CHECKS. Absolutely necessary to pass to be used in mlr3!!! -------------------

a = colnames(newdata.split1)
b = colnames(newdata.split2)
i = colnames(input)

# check if there are NA (mustn't be)
if (sum(is.na(newdata.split1)) != 0L){warning("please remove all NA from the input layers")} else {cat("No NA values observed")}

# get differetiating cols
cat("Only this class doen not appear in prediction dataset:",
    unique(i[!i %in% a]), sep = "\n")

# check if identical when class is omitted
cat("Fully identical columns",
    identical(a, i[1:length(i)-1]), sep = "\n")

cat("length of the layers:")
length(a)
length(i)

