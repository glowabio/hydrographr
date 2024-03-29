#! /bin/bash

# full path to file with snapped points
export DATA=$1
# names of columns for occurrence id, longitude and latitude
export ID=$2
export LON=$3
export LAT=$4

# full path to raster file with the direction variable
export DIRE=$5

# path to directory where to store the output(s)
export OUT=$6

# number of cores to use if in parallel
export PAR=$7

export COMPRESSION=$8

export LEVEL=$9

export BTIFF=${10}


###############################################################################
UpstreamBasin(){

    # ocurrences ids
export S=$1
    # coordinates for id
export coord=$(awk -v micid=${S} -v occname=${ID} -v lon=$LON -v lat=$LAT \
    'NR == 1 { for (i=1; i<=NF; i++) {f[$i] = i} } \
    BEGIN{OFS=",";} $(f[occname])==micid {print $(f[lon]),$(f[lat])}' $DATA)


grass -f --gtext --tmp-location  $DIRE <<'EOF'

#  read direction map
r.external --o input=$DIRE output=dir

# calculate the sub-basin
r.water.outlet --overwrite input=dir output=bf_${S} \
    coordinates=$coord

# zoom to the region of interest (only upstream basin extent)
g.region -a --o zoom=bf_${S}

#  Export the basin as tif file
r.out.gdal --o -f -c -m  createopt="COMPRESS=$COMPRESSION,ZLEVEL=$LEVEL,BIGTIFF=$BTIFF" \
    type=Int32  format=GTiff nodata=0 \
    input=bf_${S} output=$OUT/upstream_basin_${S}.tif

EOF

}
export -f UpstreamBasin

occids=$(awk -v occname=${ID}  'NR == 1 { for (i=1; i<=NF; i++) {f[$i] = i} } \
    NR > 1 {print $(f[occname])}' $DATA)
parallel -j $PAR --delay 3 UpstreamBasin ::: $occids


exit
