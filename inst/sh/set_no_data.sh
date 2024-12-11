#!/bin/bash

export DATADIR=$1
export VAR=$2
export NO_DATA=$3


# change no data value to the requested one
gdal_edit.py $DATADIR/$VAR -a_nodata $NO_DATA

# confirm the replacement by getting the changed no data value from the input file
export NO_DATA_NEW=$(gdalinfo $DATADIR/$VAR | grep "NoData Value="  | awk -F"=" '{print $2}')

# report no data value
echo "No data value was changed to" $NO_DATA_NEW "for the file" $VAR
