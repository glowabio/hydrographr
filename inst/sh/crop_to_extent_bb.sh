#!/bin/sh

# Crop raster file to a given bounding box

export R_INPUT="$1"
export XMIN=$2
export YMIN=$3
export XMAX=$4
export YMAX=$5
export R_OUTPUT="$6"

# Defines the compression type
export compression=$7

# Defines the corresponding compression level
export level=$8

# BIGTIFF=YES/NO required if output tiffs >4GiB
export bigtiff=$9


# R_INPUT is the path to a raster to crop, XMIN, YMIN, XMAX and YMAX are
# the coordinates of the corner of a bounding box
# and R_OUTPUT the path to the cropped raster

gdalwarp -te $XMIN $YMIN $XMAX $YMAX "$R_INPUT" "$R_OUTPUT" -co COMPRESS=$compression\
         -co ZLEVEL=$level -co BIGTIFF=$bigtiff -overwrite
