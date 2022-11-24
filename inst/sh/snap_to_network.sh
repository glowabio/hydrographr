#! /bin/bash

##  file (e.g. txt or csv) that has been generated at the beginning
##  of the R function, based on the data.frame table the user provided 
export DAT=$1

## names of the lon and lat columns
export LON=$2
export LAT=$3

## stream raster file (e.g. .tif file)
export STR=$4

## radius distance
export rdist=$5

## accumulation raster files
export ACC=$6

## accumulation threshold
export acct=$7

## Full path to output snap_points.txt file
export SNAP=$8

## Temporary folder
export TMPDIR=$9

## Set random string
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)

## save name of file without extension
b=$(echo $DAT | awk -F"." '{print $1}')

# Note: Tmp output from R is already a .csv
# if the file is not csv, add the comma and make it .csv
#if [ "${DAT: -4}" != ".csv" ]
#then
#    cat  $DAT | tr -s '[:blank:]' ',' > ${b}.csv
#    export DATC=$(echo ${b}.csv)
#fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $TMPDIR/ref_points_${RAND_STRING}.gpkg $DATC -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES


##  do the snapping in GRASS
grass78 -f -text --tmp-location -c $STR  <<'EOF'

v.in.ogr --o input=$TMPDIR/ref_points_${RAND_STRING}.gpkg layer=ref_points output=ref_points \
    type=point #key=

r.in.gdal input=$STR output=stream

if [ ! -z $ACC  ]
then 
    r.in.gdal input=$ACC output=accum
    r.stream.snap --o input=ref_points output=snap_points stream_rast=stream \
        radius=$rdist accumulation=accum threshold=$acct
else
    r.stream.snap --o input=ref_points output=snap_points stream_rast=stream \
        radius=$rdist 
fi

v.out.ascii -c input=snap_points separator=space | awk '{print $1, $2}' \
    > $TMPDIR/snap_coords_${RAND_STRING}.txt

sed -i 's/east/lon_snap/g' $TMPDIR/snap_coords_${RAND_STRING}.txt
sed -i 's/north/lat_snap/g' $TMPDIR/snap_coords_${RAND_STRING}.txt

paste -d" " $DAT $TMPDIR/snap_coords_${RAND_STRING}.txt >  $SNAP

EOF

rm $DIR/snap_coords_${RAND_STRING}.txt  $DIR/ref_points_${RAND_STRING}.gpkg #$DIR/$DATC
