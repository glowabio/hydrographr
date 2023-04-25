#!/bin/sh

# Crop raster file to a certain extent

export RINPUT=$1
export VECTOR=$2
export ROUTPUT=$3

# RINPUT is the path to a raster to crop, VECTOR the path to a
# VECTOR file and ROUTPUT the path to the cropped raster

# get the layer name in the VECTOR file

LAYER=$(ogrinfo -so -al $VECTOR | grep "Layer name:" | awk '{print $3}')

# crop
gdalwarp -cutline  $VECTOR  -cl $LAYER -crop_to_cutline $RINPUT \
-co COMPRESS=LZW -co ZLEVEL=9 -dstnodata -9999999 \
 $ROUTPUT -overwrite
