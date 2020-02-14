# Script for pre-procession and harmonisation of S2 and S1 data
# Konstantin Schellenberg

source("import.R")

######################################################################
# Training Dataset
######################################################################

# create input tables

# full
x = 1
if (x == 1){

    # remove files
    file.remove(c(paste0(path_rds, "learning_input_vh.rds"),
                  paste0(path_rds, "learning_input_red.rds"),
                  paste0(path_rds, "learning_input_nir.rds")))

    # make new ground truth tables
    gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
    gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
    gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")

    # read in
    vh_input = readRDS(paste0(path_rds, "learning_input_vh.rds"))
    red_input = readRDS(paste0(path_rds, "learning_input_red.rds"))
    nir_input = readRDS(paste0(path_rds, "learning_input_nir.rds"))

} else {

    # read in
    vh_input = readRDS(paste0(path_rds, "learning_input_vh.rds"))
    red_input = readRDS(paste0(path_rds, "learning_input_red.rds"))
    nir_input = readRDS(paste0(path_rds, "learning_input_nir.rds"))

}

input1 = vh_input ############### 20 Pixels missing!!!!!!!!!
input2 = red_input
input3 = nir_input

nrow(input1)
nrow(input2)
nrow(input3)

# merge data.frames, find out with cols are dublicates, coords and class column only once
input = bind_task(vh_input, red_input, nir_input)

# write to disk
write_rds(input, path = paste0(path_developement, "rda/input.rds"))

# read in
input = readRDS(paste0(path_developement, "rda/input.rds"))


######################################################################
# Prediction Dataset chunk-wise 4 regions
######################################################################

file_descriptor = "righttop"


prefix = "nir"
option = 3
naming = path_naming_s2

# transfers raster to datatable and stores it on disk
# full read-in provided via rename_bandnames

save_chunk_to_dataframe(x = paste0(path_s2, paste(prefix, file_descriptor, sep = "_"), ".tif"),
                        outfile = paste0(path_rds, name, ".rds"),
                        option = option,
                        var_prefix = prefix,
                        naming = naming)

######################################################################
# subsetting input tables (optional)
######################################################################

file_descriptor = "leftbottom"

newdata1 = readRDS(paste0(path_rds, "vh_", file_descriptor, ".rds"))
newdata2 = readRDS(paste0(path_rds, "red_", file_descriptor, ".rds"))
newdata3 = readRDS(paste0(path_rds, "nir_", file_descriptor, ".rds"))

newdata = as.data.table(bind_newdata(newdata1, newdata2, newdata3))
newdata[is.na(newdata)] = -99

begin = 1
half = length(rownames(newdata))/2
end = length(rownames(newdata))

newdata.split1 = newdata[begin:half,] # upper
newdata.split2 = newdata[half:end,] # lower

# optional writing to disk
c = 1
if (c == 1){
    write_rds(newdata.split1, path = paste0(path_rds, file_descriptor, "_split1.rds"))
    write_rds(newdata.split2, path = paste0(path_rds, file_descriptor, "_split2.rds"))

    ### read
    newdata.split1 = readRDS(paste0(path_rds, file_descriptor, "_split1.rds"))
    newdata.split2 = readRDS(paste0(path_rds, file_descriptor, "_split2.rds"))
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

