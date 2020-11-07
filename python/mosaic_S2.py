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

# filename = "F:/geodata/geo402/S2/xx_S2_indices/stack_evi/S2A_MSIL1C_20151229T075332_N0201_R135_T35JMH_mskd.vrt"
#
# from pyroSAR import identify
# id = identify(filename)
# print(id.outname_base())

def main():

    dirs = ["stack_savi"] #, "stack_ndvi", "stack_msavi", "stack_reip", "stack_rvi", "stack_dvi"]

    resolution = [30, 30]

    # shapefile (for stack boundaries)
    shp = 'F:/geodata/geo402/02_features/LADYBRAND_final_enlarged_study_area.shp'

    # store results in separate files or one single stack file? If separate then dstfile is used as a directory.
    sep = True

    for dir in dirs:

        # define input directory containing files to be stacked
        dir_in = 'F:/geodata/geo402/S2/xx_S2_indices/' + dir
        print(dir_in)
        os.makedirs(dir_in, exist_ok=True)

        # define output file name
        dstfile = 'F:/geodata/geo402/S2/xx_S2_indices/mosaics/' + dir
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
              targetres=resolution, srcnodata=-9999, dstnodata=-9999,
              shapefile=shp, sortfun=seconds, separate=sep, overwrite=True)
        # -tr 30 30 -te 463563.375 6739018 549706.4375 6791364

if __name__ == '__main__':
    main()

