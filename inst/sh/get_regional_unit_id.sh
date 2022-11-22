#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export REG_UNIT_LAYER=$4
export TMPDIR=$5


# add header
echo "reg_unit_id" > $TMPDIR/reg_unit_ids.txt

# Query the global file of the regional units
awk -v LON=$LON -v LAT=$LAT '
NR==1 {
    for (i=1; i<=NF; i++) {
        f[$i] = i
    }
}
{ if(NR>1) {print $(f[LON]), $(f[LAT]) }}
' $DATA   | gdallocationinfo -valonly -geoloc  $REG_UNIT_LAYER | sort -n -u >> $TMPDIR/reg_unit_ids.txt



# if they give points, first gdallocationinfo on the regional_unit_ovr.tif
# for tiles ID: lookup table with tileID and reg.unit IDs.
#  then  check in which tiles they fall


# if they give extent, crop the regional_unit_ovr.tif with
# gdal_translate
# gdalinfo -hist / pkstat -hist on the cropped file | grep -V " 0" ....

# i think that in the case of the extent, creating a vrt is still the fastest way to get the tile ids
# but for cu ids?
