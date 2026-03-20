#!/bin/sh
# Crop vector file to a given bounding box
export V_INPUT=$1
export XMIN=$2
export YMIN=$3
export XMAX=$4
export YMAX=$5
export V_OUTPUT=$6
export FORMAT=$7
# V_INPUT is the path to a vector file to crop, XMIN, YMIN, XMAX and YMAX are
# the coordinates of the corners of a bounding box
# and V_OUTPUT the path to the cropped vector
ogr2ogr -f "$FORMAT" -clipsrc $XMIN $YMIN $XMAX $YMAX $V_OUTPUT $V_INPUT -overwrite
