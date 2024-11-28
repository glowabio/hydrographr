#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export SUBC_LAYER=$4
export BASIN_LAYER=$5
export TMPDIR=$6
export IDSDIR=$7
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)	

# Documentation of input params:
# DATA: Text file with two named, space separated columns for longitude and latitude.
# LON:  Name of the longitude column.
# LAT:  Name of the latitude column.
# SUBC_LAYER:  Path to the raster file from which to get the subcatchment ids, or "0".
# BASIN_LAYER: Path to the raster file from which to get the subcatchment ids, or "0".
# TMPDIR: Path to a directory where gdal will store its immediate results, i.e. the extracted subcatchment_ids and/or basin_ids will be stored temporarily (will be removed before finishing).
# IDSDIR: Path to the file where to store the final result, which is the text file in DATA with one or two columns added ("subcatchment" and/or "basin").


if [ "$SUBC_LAYER" = "0"  ] ; then

    # add header to temporary result file:
    echo "basin_id" > $TMPDIR/subc_ids_${RAND_STRING}.txt

    # run gdallocationinfo (will write result to temporary result file):
    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/subc_ids_${RAND_STRING}.txt

    # merge input lon-lat columns with temporary result file to one result file
	paste -d" " $DATA $TMPDIR/subc_ids_${RAND_STRING}.txt > $IDSDIR

    # remove temporary result file
	rm $TMPDIR/subc_ids_${RAND_STRING}.txt

elif [ "$BASIN_LAYER" = "0"  ] ; then

    # add header to temporary result file:
    echo "subcatchment_id" > $TMPDIR/basin_ids_${RAND_STRING}.txt

    # run gdallocationinfo (will write result to temporary result file):
    awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/basin_ids_${RAND_STRING}.txt

    # merge input lon-lat columns with temporary result file to one result file
    paste -d" " $DATA $TMPDIR/basin_ids_${RAND_STRING}.txt > $IDSDIR

    # remove temporary result file
    rm  $TMPDIR/basin_ids_${RAND_STRING}.txt

else
    # add header to temporary result file:
   echo "subcatchment_id" > $TMPDIR/subc_ids_${RAND_STRING}.txt

    # run gdallocationinfo (will write result to temporary result file):
   awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
            }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC_LAYER >> $TMPDIR/subc_ids_${RAND_STRING}.txt

    # add header to temporary result file
    echo "basin_id" > $TMPDIR/basin_ids_${RAND_STRING}.txt

    # run gdallocationinfo (will write result to temporary result file):
    awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
             }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN_LAYER >> $TMPDIR/basin_ids_${RAND_STRING}.txt

    # merge input lon-lat columns with both temporary result files to one result file
    paste -d" " $DATA $TMPDIR/subc_ids_${RAND_STRING}.txt $TMPDIR/basin_ids_${RAND_STRING}.txt > $IDSDIR

    # remove both temporary result files
    rm $TMPDIR/subc_ids_${RAND_STRING}.txt $TMPDIR/basin_ids_${RAND_STRING}.txt
 fi

