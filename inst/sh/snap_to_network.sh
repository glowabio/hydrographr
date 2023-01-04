#! /bin/bash

##  file (e.g. txt or csv) that has been generated at the beginning
##  of the R function, based on the data.frame table the user provided
export DATA=$1

## names of the lon and lat columns
export LON=$2
export LAT=$3

## stream raster file (e.g. .tif file)
export STR=$4

## accumulation raster files
export ACC=$5

## What to calculate
export CALC=$6

## radius distance
export rdist=$7

## accumulation threshold
export acct=$8

## Full path to output snap_points.txt file
export SNAP=$9

## Temporary folder
export DIR=${10}



## Set random string
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)

## save name of file without extension
#b=$(echo $DAT | awk -F"." '{print $1}')

# Note: Tmp output from R is already a .csv
# if the file is not csv, add the comma and make it .csv
#if [ "${DAT: -4}" != ".csv" ]
#then
#    cat  $DAT | tr -s '[:blank:]' ',' > ${b}.csv
#    export DATC=$(echo ${b}.csv)
#fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points_${RAND_STRING}.gpkg $DATA -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES


##  do the snapping in GRASS
grass -f --gtext --tmp-location $STR  <<'EOF'

v.in.ogr --o input=$DIR/ref_points_${RAND_STRING}.gpkg layer=ref_points output=ref_points \
    type=point #key=

r.in.gdal input=$STR output=stream


if [ "$CALC" = "dist" ]
then

    r.stream.snap --o input=ref_points output=snap_points stream_rast=stream \
        radius=$rdist

    r.what map=stream points=snap_points separator=comma \
        | awk -F, '{print $4}' > $DIR/stream_ID_${RAND_STRING}.txt

    v.out.ascii -c input=snap_points separator=space | awk '{print $1, $2}' \
        > $DIR/snap_coords_${RAND_STRING}.txt

    sed -i 's/east/lon_snap/g' $DIR/snap_coords_${RAND_STRING}.txt
    sed -i 's/north/lat_snap/g' $DIR/snap_coords_${RAND_STRING}.txt

    cat  $DATA | tr -s ',' ' ' > $DIR/coords_${RAND_STRING}.txt
    paste -d" " $DIR/coords_${RAND_STRING}.txt $DIR/snap_coords_${RAND_STRING}.txt \
        <(printf "%s\n" subc_id_snap $(cat $DIR/stream_ID_${RAND_STRING}.txt))  \
        >  $SNAP

    rm $DIR/snap_coords_${RAND_STRING}.txt $DIR/stream_ID_${RAND_STRING}.txt \
        $DIR/ref_points_${RAND_STRING}.gpkg
fi


if [ "$CALC" = "accu" ]
then
    r.in.gdal input=$ACC output=accum
    r.stream.snap --o input=ref_points output=snap_points stream_rast=stream \
        radius=$rdist accumulation=accum threshold=$acct

    r.what map=stream points=snap_points separator=comma \
        | awk -F, '{print $4}' > $DIR/stream_ID_${RAND_STRING}.txt

    v.out.ascii -c input=snap_points separator=space | awk '{print $1, $2}' \
        > $DIR/snap_coords_${RAND_STRING}.txt

    sed -i 's/east/lon_snap/g' $DIR/snap_coords_${RAND_STRING}.txt
    sed -i 's/north/lat_snap/g' $DIR/snap_coords_${RAND_STRING}.txt

    cat  $DATA | tr -s ',' ' ' > $DIR/coords_${RAND_STRING}.txt
    paste -d" " $DIR/coords_${RAND_STRING}.txt $DIR/snap_coords_${RAND_STRING}.txt \
        <(printf "%s\n" subc_id_snap $(cat $DIR/stream_ID_${RAND_STRING}.txt))  \
        >  $SNAP

    rm $DIR/snap_coords_${RAND_STRING}.txt $DIR/stream_ID_${RAND_STRING}.txt \
        $DIR/ref_points_${RAND_STRING}.gpkg
fi


if [ "$CALC" = "both" ]
then

    r.stream.snap --o input=ref_points output=snap_points_d stream_rast=stream \
        radius=$rdist

    r.in.gdal input=$ACC output=accum
    r.stream.snap --o input=ref_points output=snap_points_a stream_rast=stream \
        radius=$rdist accumulation=accum threshold=$acct

    r.what map=stream points=snap_points_d separator=comma \
        | awk -F, '{print $4}' > $DIR/stream_ID_${RAND_STRING}_d.txt

    r.what map=stream points=snap_points_a separator=comma \
        | awk -F, '{print $4}' > $DIR/stream_ID_${RAND_STRING}_a.txt

    v.out.ascii -c input=snap_points_d separator=space | awk '{print $1, $2}' \
        > $DIR/snap_coords_${RAND_STRING}_d.txt

    v.out.ascii -c input=snap_points_a separator=space | awk '{print $1, $2}' \
        > $DIR/snap_coords_${RAND_STRING}_a.txt

    sed -i 's/east/lon_snap_dist/g' $DIR/snap_coords_${RAND_STRING}_d.txt
    sed -i 's/north/lat_snap_dist/g' $DIR/snap_coords_${RAND_STRING}_d.txt

    sed -i 's/east/lon_snap_accu/g' $DIR/snap_coords_${RAND_STRING}_a.txt
    sed -i 's/north/lat_snap_accu/g' $DIR/snap_coords_${RAND_STRING}_a.txt

    cat  $DATA | tr -s ',' ' ' > $DIR/coords_${RAND_STRING}.txt

    paste -d" " $DIR/coords_${RAND_STRING}.txt \
        $DIR/snap_coords_${RAND_STRING}_d.txt \
        <(printf "%s\n" subc_id_snap_dist $(cat $DIR/stream_ID_${RAND_STRING}_d.txt))  \
        $DIR/snap_coords_${RAND_STRING}_a.txt \
        <(printf "%s\n" subc_id_snap_accu $(cat $DIR/stream_ID_${RAND_STRING}_a.txt))  \
        >  $SNAP

    rm $DIR/snap_coords_${RAND_STRING}_*.txt $DIR/stream_ID_${RAND_STRING}_*.txt \
        $DIR/ref_points_${RAND_STRING}.gpkg
fi


EOF
