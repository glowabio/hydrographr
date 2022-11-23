#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export SUBC_LAYER=$4
export BASIN_LAYER=$5
export TMPDIR=$6
export IDSDIR=$7
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)	

if [ "$SUBC_LAYER" = "0"  ] ; then

    # add header
    echo "basin_id" > $TMPDIR/subc_ids_${RAND_STRING}.txt

    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/subc_ids_${RAND_STRING}.txt

	paste -d" " $DATA $TMPDIR/subc_ids_${RAND_STRING}.txt > $IDSDIR
	rm $TMPDIR/subc_ids_${RAND_STRING}.txt

elif [ "$BASIN_LAYER" = "0"  ] ; then

    # add header
    echo "subcatchment_id" > $TMPDIR/basin_ids_${RAND_STRING}.txt

    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/basin_ids_${RAND_STRING}.txt

    paste -d" " $DATA $TMPDIR/basin_ids_${RAND_STRING}.txt > $IDSDIR
    rm  $TMPDIR/basin_ids_${RAND_STRING}.txt

else
   echo "subcatchment_id" > $TMPDIR/subc_ids_${RAND_STRING}.txt
   awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
            }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/subc_ids_${RAND_STRING}.txt

    echo "basin_id" > $TMPDIR/basin_ids_${RAND_STRING}.txt
    awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
             }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/basin_ids_${RAND_STRING}.txt

    paste -d" " $DATA $TMPDIR/subc_ids_${RAND_STRING}.txt $TMPDIR/basin_ids_${RAND_STRING}.txt > $IDSDIR
    rm $TMPDIR/subc_ids_${RAND_STRING}.txt $TMPDIR/basin_ids_${RAND_STRING}.txt
 fi

