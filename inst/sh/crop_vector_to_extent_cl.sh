#!/bin/sh
# Crop vector file using a clip layer (polygon boundary)
export V_INPUT=$1
export CLIP=$2
export V_OUTPUT=$3
export FORMAT=$4
# V_INPUT is the path to a vector file to crop, CLIP the path to a
# polygon vector file and V_OUTPUT the path to the cropped vector
ogr2ogr -f "$FORMAT" -clipsrc $CLIP $V_OUTPUT $V_INPUT -overwrite
