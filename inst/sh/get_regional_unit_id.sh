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

