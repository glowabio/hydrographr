#! /bin/bash

#################################################

##   directory where the data is located
export DIR=$1

##   file (e.g. txt or csv) that has been generated at the beginning
##   of the R function, based on the data.frame table the user provided 
export DAT=$2

## names of the lon and lat columns
export LON=$3
export LAT=$4

## stream raster file (e.g. .tif file)
export STR=$5

##  radius distance
export rdist=$6

##  accumulation raster files
export ACC=$7

## accumulation threshold
export acct=$8


## save name of file without extension
b=$(echo $DAT | awk -F"." '{print $1}')

## if the file is not csv, add the comma and make it .csv
if [ "${DAT: -4}" != ".csv" ]
then
    cat  $DAT | tr -s '[:blank:]' ',' > ${b}.csv
    export DATC=$(echo ${b}.csv)
fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points.gpkg $DATC -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES


##  do the snapping in GRASS
grass78 -f -text --tmp-location -c $STR  <<'EOF'

v.in.ogr --o input=$DIR/ref_points.gpkg layer=ref_points output=ref_points \
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
    > $DIR/snap_coords.txt

sed -i 's/east/lon_snap/g' $DIR/snap_coords.txt
sed -i 's/north/lat_snap/g' $DIR/snap_coords.txt

paste -d" " $DAT $DIR/snap_coords.txt >  $DIR/${b}_snap.txt

EOF

rm $DIR/snap_coords.txt $DIR/$DATC $DIR/ref_points.gpkg
