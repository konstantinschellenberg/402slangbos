# Stacking raster data for classification

# general settings:
# Res: 30m
# Proj: EPSG:32735

# (1) filter for EO layer > 80% coverage
# (2) change no-data -> 0
# (3) stack the layers to ENVI file

import subprocess as sp
from osgeo import gdal
# import rasterio as rio

# load paths of the rasters

vh = "D:/Geodaten/GEO402/01_data/S1_GRD/S1A_IW_GRD_VH_stack"

co_no_ref = "D:/Geodaten/GEO402/01_data/S1_SLC/S1_A_VV_stack_coherence_full_area"
co_nodata0 = "D:/Geodaten/GEO402/01_data/S1_SLC/S1A_IW_SLC_VV_stack_nodata0.vrt"
co = "D:/Geodaten/GEO402/01_data/S1_SLC/S1A_IW_SLC_VV_stack"

ndvi_vrt = "D:/Geodaten/GEO402/01_data/S2/stack_ndvi.vrt"
savi_vrt = "D:/Geodaten/GEO402/01_data/S2/stack_savi.vrt"

ndvi = "D:/Geodaten/GEO402/01_data/S2/ndvi"
savi = "D:/Geodaten/GEO402/01_data/S2/savi"

cube = "D:/Geodaten/GEO402/01_data/stack/datacube.envi"
cube_filled = "D:/Geodaten/GEO402/01_data/stack/datacube_filled"
cube_vrt = "D:/Geodaten/GEO402/01_data/stack/datacube.vrt"

mask = "D:/Geodaten/GEO402/01_data/mask.tif"

# SETTINGS
xmin = 463563.429
ymin = 6739014.384
xmax = 549693.429
ymax = 6791364.384

# very small subset
# xmin = 463563.429
# ymin = 6780000.384
# xmax = 470000.429
# ymax = 6791364.384

resolution = 30
srcnodata = [0, -99, -9999]
dstnodata = 0
EPSG = "EPSG:32735"
memory = 256000

# INFORMATION
for ele in [vh, co, ndvi, savi]:
       # print(ele)
       srs_info = f"gdalsrsinfo {ele}"
       # sp.run(srs_info)
       
# sp.run(f"gdalinfo {cube}")
       
# PROCESS

### Reproj SLC
# let's reproject the SLC, crop and set resolution, srcnodata = 0:
reproj = f"gdalwarp -t_srs {EPSG} -srcnodata {srcnodata[0]} -dstnodata {dstnodata} -tr {resolution} {resolution} " \
         f"-te {xmin} {ymin} {xmax} {ymax} -wm {memory} -overwrite -multi -of VRT {co_no_ref} {co_nodata0}"

# output as ENVI file, srcnodata = -99 -> all nodatas are cleaned -> now moving to R to stack to datacube
reproj2 = f"gdalwarp -srcnodata {srcnodata[1]} -dstnodata {dstnodata}" \
         f" -wm {memory} -overwrite -multi -of ENVI {co_nodata0} {co}"

# print(reproj + "\n", reproj2)

# sp.run(reproj)
# sp.run(reproj2)

#### NDVI AND SAVI as ENVI
# Writing out ndvi and savi datasets as ENVI file

ndvi_stacking = f"gdal_translate -of ENVI {ndvi_vrt} {ndvi}"
savi_stacking = f"gdal_translate -of ENVI {savi_vrt} {savi}"

# sp.run(ndvi_stacking)
# sp.run(savi_stacking)

#### FILLING NA

expression = "where(B==0,0,A)"

# also takes in an algorithm
'''
1. Find our nodata value (most likely -3.4e35 for float32
2. Creat mask by raster::rasterize or gdal_rasterize (same) with 1 = data, 0 = data discarded
3. Use gdal_calc expression "where(B==0,0,A)" to determine everything 0 where there is no data in A
'''
# edit = f"python C:\Programme\GDAL\gdal_edit.py {cube}"
fill = f'python C:\Programme\GDAL\gdal_calc.py --calc {expression}'\
       f' --format ENVI -A {cube} --allBands A -B {mask} --B_band 1 --overwrite --outfile {cube_filled}'

# sp.run(edit)
sp.run(fill)
#

#### depr: stacking 4 dense layers
stack = f"gdalbuildvrt -te {xmin} {ymin} {xmax} {ymax} -tr {resolution} {resolution} " \
       f" -separate -vrtnodata 0 -overwrite {cube} {vh} {co} {ndvi} {savi}"

# stacking does not work for all layers!
# sp.run(stack)


