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


def main():
    # define input directory containing file sto be stacked
    dir_in = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/02_processed_gamma/Free State/proc_out/'
    
    # define output file name
    dstfile = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/03_stacks/S1_A_D_VH_stack_base_period_03_2015_03_2018_free_state_tile_1'
    
    # shapefile (for stack boundaries)
    shp = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/xx_study_area/SALDI_06_Free_State_fishnet_tile_1.shp'
    
    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = False
    
    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['S1*2015*VH*db.tif'])
    srcfiles.extend(finder(dir_in, ['S1*2016*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*2017*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201801*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201802*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201803*VH*db.tif']))
    print(dstfile)
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
          targetres=[10, 10], srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=False)


if __name__ == '__main__':
    main()

def main():
    # define input directory containing file sto be stacked
    dir_in = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/02_processed_gamma/Free State/proc_out/'
    
    # define output file name
    dstfile = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/03_stacks/S1_A_D_VH_stack_base_period_03_2015_03_2018_free_state_tile_2'
    
    # shapefile (for stack boundaries)
    shp = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/xx_study_area/SALDI_06_Free_State_fishnet_tile_2.shp'
    
    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = False
    
    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['S1*2015*VH*db.tif'])
    srcfiles.extend(finder(dir_in, ['S1*2016*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*2017*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201801*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201802*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201803*VH*db.tif']))
    print(dstfile)
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
          targetres=[10, 10], srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=False)


if __name__ == '__main__':
    main()
	

def main():
    # define input directory containing file sto be stacked
    dir_in = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/02_processed_gamma/Free State/proc_out/'
    
    # define output file name
    dstfile = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/03_stacks/S1_A_D_VH_stack_base_period_03_2015_03_2018_free_state_tile_3'
    
    # shapefile (for stack boundaries)
    shp = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/xx_study_area/SALDI_06_Free_State_fishnet_tile_3.shp'
    
    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = False
    
    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['S1*2015*VH*db.tif'])
    srcfiles.extend(finder(dir_in, ['S1*2016*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*2017*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201801*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201802*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201803*VH*db.tif']))
    print(dstfile)
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
          targetres=[10, 10], srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=False)


if __name__ == '__main__':
    main()
	

def main():
    # define input directory containing file sto be stacked
    dir_in = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/02_processed_gamma/Free State/proc_out/'
    
    # define output file name
    dstfile = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/06_Free_State/03_stacks/S1_A_D_VH_stack_base_period_03_2015_03_2018_free_state_tile_4'
    
    # shapefile (for stack boundaries)
    shp = '/geonfs01_vol1/01_EMS_SALDI_KNP_S1/01_S1_GRD/02_SALDI/xx_study_area/SALDI_06_Free_State_fishnet_tile_4.shp'
    
    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = False
    
    # list files to be resampled; those not overlapping with the shapefile geometry will excluded by function stack
    srcfiles = finder(dir_in, ['S1*2015*VH*db.tif'])
    srcfiles.extend(finder(dir_in, ['S1*2016*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*2017*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201801*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201802*VH*db.tif']))
    srcfiles.extend(finder(dir_in, ['S1*201803*VH*db.tif']))
    print(dstfile)
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
          targetres=[10, 10], srcnodata=-99, dstnodata=-99,
          shapefile=shp, sortfun=seconds, separate=sep, overwrite=False)


if __name__ == '__main__':
    main()




