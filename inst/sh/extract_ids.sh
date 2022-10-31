#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export LAYER=$4

# The $LAYER is a path either to the subcatchment or to the basin id layer

awk -v LON=$LON -v LAT=$LAT '
NR==1 {
    for (i=1; i<=NF; i++) {
        f[$i] = i
    }
}
{ if(NR>1) {print $(f[LON]), $(f[LAT]) }}
' $DATA   | gdallocationinfo -valonly -geoloc  $LAYER



