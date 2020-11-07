##############################################################
# demonstration script for resampling, stacking, mosaicking and subsetting SAR images
# John Truckenbrodt 2017
##############################################################
import sys
#sys.path.insert(0, "/geonfs01_vol1/qe89hep/spatialist")
#sys.path = ['/geonfs01_vol1/qe89hep/spatialist', '/geonfs02_vol2/software/local/lib/python2.7/site-packages/GDAL-2.2.1-py2.7-linux-x86_64.egg','/usr/local/lib/python2.7/site-packages/GDAL-2.0.0-py2.7-linux-x86_64.egg', '/geonfs02_vol2/software/local/lib/python2.7/site-packages', '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/xx_2_mosaicing', '/usr/local/lib/python27.zip', '/usr/local/lib/python2.7', '/usr/local/lib/python2.7/plat-linux2', '/usr/local/lib/python2.7/lib-tk', '/usr/local/lib/python2.7/lib-old', '/usr/local/lib/python2.7/lib-dynload', '/homes2/geoinf/c3urma/.local/lib/python2.7/site-packages', '/usr/local/lib/python2.7/site-packages']

import os

from pyroSAR.ancillary import groupbyTime, seconds
from spatialist import stack
from spatialist.ancillary import finder
from pyroSAR.drivers import identify

def main():
    '''
    both polarisations are included in the script but are stored in different folders
    '''

    resolution = [30, 30]

    # shapefile (for stack boundaries)
    shp = 'F:/geodata/geo402/##study_area/LADYBRAND_final_enlarged_study_area.shp'

    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = False

    # define input directory containing files to be stacked
    dir_in = 'F:/geodata/geo402/S1_GRD/xx_new/GRD_VH_vrts/'
    print(dir_in)
    # os.makedirs(dir_in, exist_ok=True)

    # define output file name
    dstfile = 'F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VH_stack'
    print(dstfile)

    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['*'])

    # check whether dstfile is already a file
    if os.path.isfile(dstfile):
        raise IOError('dstfile already exists')

    # create groups of similar time stamps for mosaicking.
    # All images with a time stamp of less than 30s difference will be grouped
    groups = groupbyTime(srcfiles, seconds, 30)

    # final function call
    # groups will be mosaicked first
    # the resulting images will all have the same extent
    stack(srcfiles=groups, dstfile=dstfile, resampling='bilinear',
          targetres=resolution, srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=True,
          cores=7)


    ### VV

    # define input directory containing files to be stacked
    dir_in = 'F:/geodata/geo402/S1_GRD/xx_new/GRD_VV_vrts/'
    print(dir_in)
    # os.makedirs(dir_in, exist_ok=True)

    # define output file name
    dstfile = 'F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack'
    print(dstfile)

    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['*'])

    # check whether dstfile is already a file
    if os.path.isfile(dstfile):
        raise IOError('dstfile already exists')

    # create groups of similar time stamps for mosaicking.
    # All images with a time stamp of less than 30s difference will be grouped
    groups = groupbyTime(srcfiles, seconds, 30)

    # final function call
    # groups will be mosaicked first
    # the resulting images will all have the same extent
    stack(srcfiles=groups, dstfile=dstfile, resampling='bilinear',
          targetres=resolution, srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=True,
          cores=7)

if __name__ == '__main__':
    main()

# gdalbuildvrt -r nearest -srcnodata "-99" -tr 0.001 0.001 -te 26.6241841334384013 -29.4777883287671010
# 27.5127027974126008 -29.0049152907063998 -input_file_list F:/geodata/geo402/S1_SLC/xx_new/inputfiles.txt
# S1A_IW_SLC_stack.vrt

# gdalwarp -s_srs EPSG:4326 -t_srs EPSG:32735 -tr 30 30 -te 26.6241841334384013 -29.4777883287671010 27.5127027974126008 -29.0049152907063998 -te_srs EPSG:4326 -overwrite /SLCs/*.tif output.vrt

# writing input file list for gdalbuildvrt
# with open('F:/geodata/geo402/S1_GRD/xx_new/S1A_IW_GRD_VV_stack.txt', 'w') as filehandle:
#     for listitem in groups:
#         filehandle.write('%s\n' % listitem)
