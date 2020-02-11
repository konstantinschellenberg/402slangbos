# Script for preprocession and hamonisation of S2 and S1 data
# Konstantin Schellenberg

# TRAINING
######################################################################
# Loading Libraries & refer to import source
######################################################################

source("import.R")
getOption("max.print")
options(max.print = 1000)

######################################################################
# Training Dataset
######################################################################

# create input tables

# full
x = 0
if (x == 1){
    file.remove(c(paste0(rds_path, "learning_input_vh.rds"),
                  paste0(rds_path, "learning_input_red.rds"),
                  paste0(rds_path, "learning_input_nir.rds")))
    gt_from_raster(raster = vh, train_data = gt, response_col = "Name", outfile = "vh")
    gt_from_raster(raster = red, train_data = gt, response_col = "Name", outfile = "red")
    gt_from_raster(raster = nir, train_data = gt, response_col = "Name", outfile = "nir")
}

# gt_from_raster(raster = vv, train_data = gt, response_col = "Name", outfile = "vv")
# # small
# gt_from_raster(raster = vh_small, train_data = gt_smaller, response_col = "Name", outfile = "vh_sm")
# gt_from_raster(raster = red_small, train_data = gt_smaller, response_col = "Name", outfile = "red_sm")
# gt_from_raster(raster = nir_small, train_data = gt_smaller, response_col = "Name", outfile = "nir_sm")


# load input tables

# full
vh_input = readRDS(paste0(rds_path, "learning_input_vh.rds"))
red_input = readRDS(paste0(rds_path, "learning_input_red.rds"))
nir_input = readRDS(paste0(rds_path, "learning_input_nir.rds"))

# vv_input = readRDS(paste0(rds_path, "learning_input_vv.rds"))
# # small
# vh_input = readRDS(paste0(rds_path, "learning_input_vh_sm.rds"))
# red_input = readRDS(paste0(rds_path, "learning_input_red_sm.rds"))
# nir_input = readRDS(paste0(rds_path, "learning_input_nir_sm.rds"))


# deleting previous:


# merge data.frames, find out with cols are dublicates, coords and class column only once

input = bind_task(vh_input, red_input, nir_input)
write_rds(input, path = paste0(path_developement, "rda/input.rds"))

input = readRDS(paste0(path_developement, "rda/input.rds"))

######################################################################
# Prediction Dataset
######################################################################

# data = as.data.table.raster(vh, xy = TRUE, inmem = F)
# 4 extent instances for the prediction to fit into memory

coords = st_bbox(study_area)

width = (coords$xmax - coords$xmin) / 2
height = (coords$ymax - coords$ymin) / 2

xmin = coords$xmin
ymin = coords$ymin
xmax = width + coords$xmin
ymax = height + coords$ymin


tr = cbind(xmin, ymin, xmax, ymax)
tr2 = cbind(xmin = tr[,1], ymin = tr[,2] + height, xmax = tr[,3], ymax = tr[,4] + height)
tr3 = cbind(xmin = tr[,1] + width, ymin = tr[,2], xmax = tr[,3] + width, ymax = tr[,4])
tr4 = cbind(xmin = tr[,1] + width, ymin = tr[,2] + height, xmax = tr[,3] + width, ymax = tr[,4] + height)

tr1 = c(tr)
tr2 = c(tr2)
tr3 = c(tr3)
tr4 = c(tr4)

tr1
tr2
tr3
tr4

tr = list(tr1, tr2, tr3, tr4)

## Wrapper for gdal translate:
# params: raster, extent(tr=list of extents in format: xmin, ymin, xmax, ymax), filename

# batch processing, run with b = 1
b = 1

if (b ==1){
    for (i in seq_along(tr)){

        if(i==1){outname = "vh_leftbottom"} else if (i==2){outname = "vh_lefttop"}
        else if (i==3){outname = "vh_rightbottom"} else if (i==4){outname = "vh_righttop"} else {warning("fail")}

        warp_tiles(raster = s1vh_path, extent = tr[[i]], outname = outname)
        print(i)
        print(outname)
        print(tr[[i]])
    }
}


lb_vh.in = brick(paste0(path_s2, "vh_leftbottom.tif")) %>% rename_bandnames(option = 1, var_prefix = "vh", naming = olds1)
lb_red.in = brick(paste0(path_s2, "red_leftbottom.tif")) %>% rename_bandnames(option = 3, var_prefix = "red", naming = paste0(path_s2, "bandnames_less20.txt"))
lb_nir.in = brick(paste0(path_s2, "nir_leftbottom.tif")) %>% rename_bandnames(option = 3, var_prefix = "nir", naming = paste0(path_s2, "bandnames_less20.txt"))

lb_vh = as.data.table.raster(lb_vh.in, xy = TRUE, inmem = FALSE)
lb_red = as.data.table.raster(lb_red.in, xy = TRUE, inmem = FALSE)
lb_nir = as.data.table.raster(lb_nir.in, xy = TRUE, inmem = FALSE)

write_rds(lb_vh, path = paste0(path_developement, "rda/vh_datatable.rds"))
write_rds(lb_red, path = paste0(path_developement, "rda/red_datatable.rds"))
write_rds(lb_nir, path = paste0(path_developement, "rda/nir_datatable.rds"))

input1 = readRDS(paste0(path_developement, "rda/vh_datatable.rds"))
input2 = readRDS(paste0(path_developement, "rda/red_datatable.rds"))
input3 = readRDS(paste0(path_developement, "rda/nir_datatable.rds"))

newdata = as.data.table(bind_newdata(input1, input2, input3))

write_rds(newdata, path = paste0(path_developement, "rda/newdata_datatable.rds"))



# loading and subsetting newdata -----------------------------------------------
newdata = readRDS(paste0(path_developement, "rda/newdata_datatable.rds"))
class(newdata)

a = as.data.table(newdata)

begin = 1
half = length(rownames(a))/2
end = length(rownames(a))

newdata.split1 = a[half:end,]
newdata.split2 = a[begin:half,]

write_rds(newdata.split1, path = paste0(path_developement, "rda/newdata_split1_datatable.rds"))
write_rds(newdata.split2, path = paste0(path_developement, "rda/newdata_split2_datatable.rds"))

### read
newdata.split1 = readRDS(paste0(path_developement, "rda/newdata_split1_datatable.rds"))
newdata.split2 = readRDS(paste0(path_developement, "rda/newdata_split2_datatable.rds"))


# double-check variables (columns) with input ----------------------------------

a = colnames(newdata.split1)
b = colnames(newdata.split2)
i = colnames(input)

# check NA
if (sum(is.na(input)) != 0L){warning("please remove all NA from the input layers")}

# get differetiating cols
unique(i[!i %in% a])

# check if identical when class is omitted
identical(a, i[1:length(i)-1])


# originally from mlr3.R
input = as.data.table(readRDS(paste0(path_developement, "rda/input.rds"))) %>% dplyr::select(contains("vh"), "x", "y", "class")
# newdata = readRDS(paste0(path_developement, "rda/newdata_datatable.rds"))
newdata.split1 = readRDS(paste0(path_developement, "rda/newdata_split1_datatable.rds")) %>% dplyr::select(contains("vh"), "x", "y")
newdata.split2 = readRDS(paste0(path_developement, "rda/newdata_split2_datatable.rds"))

length(input)
length(newdata.split1)
