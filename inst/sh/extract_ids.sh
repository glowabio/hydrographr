#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export SUBC_LAYER=$4
export BASIN_LAYER=$5
export TMPDIR=$6

if [ "$SUBC_LAYER" = "0"  ] ; then

    # add header
    echo "basin_id" > $TMPDIR/ids.txt

    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/ids.txt

elif [ "$BASIN_LAYER" = "0"  ] ; then

    # add header
    echo "subcatchment_id" > $TMPDIR/ids.txt

    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/ids.txt

else
   echo "subcatchment_id" > $TMPDIR/subc_ids.txt
   awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
            }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/subc_ids.txt

    echo "basin_id" > $TMPDIR/basin_ids.txt
    awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
             }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/basin_ids.txt

    paste -d" " $DATA $TMPDIR/subc_ids.txt $TMPDIR/basin_ids.txt > $TMPDIR/ids.txt

 fi

