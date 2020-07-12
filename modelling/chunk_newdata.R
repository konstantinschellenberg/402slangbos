# script to cut newdata into digestible chunks in order to fit in mlr3 prediction

data = as.data.table.raster(vh, xy = TRUE, inmem = F)
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

## Wrapper for gdal translate to retrieve chunks of input raster (needed to process in memory):
# params: raster, extent(tr=list of extents in format: xmin, ymin, xmax, ymax), filename

# batch processing, run with b = 1
b = 1

#warp_tile's outfile path is path_s2 by default!
if (b == 1){
    for (i in seq_along(tr)){

        if(i==1){outname = "vh_leftbottom"} else if (i==2){outname = "vh_lefttop"}
        else if (i==3){outname = "vh_rightbottom"} else if (i==4){outname = "vh_righttop"} else {warning("fail")}

        warp_tiles(raster = s1vh_path, extent = tr[[i]], outname = outname)
        print(i)
        print(outname)
        print(tr[[i]])
    }
}
