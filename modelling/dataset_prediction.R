# Script for pre-procession and harmonisation of S2, S1 backscatter and S1 coherences
# Konstantin Schellenberg

source("import.R")


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

