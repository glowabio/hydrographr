#! /bin/bash

#export DIR=/home/marquez/hydrographr
#export DAT=spdata_1264942_ids_snap.txt
#export LON=lon_snap
#export LAT=lat_snap

# path - location of input/output data
export DIR=$1

# name of input table. Must be a .csv file
export DATA=$2

# name of lon and lat coordinates
export LON=$3
export LAT=$4

# full path and name of output file $DIR/pairwise_euclidian_dist.txt
export OUT=$5

################################
################################

## save name of file without extension
export b=$(echo $DATA | awk -F"." '{print $1}')

## if the file is not csv, add the comma and make it .csv
if [ "${DATA: -4}" != ".csv" ]
then
    cat  $DATA | tr -s '[:blank:]' ',' > ${b}.csv
    export DATAC=$(echo ${b}.csv)
else
    DATAC=$DATA
fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points.gpkg $DATA -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES

# Name of column for unique ID
export SITE=$( awk -F, 'NR==1 {print $1}' $DATA )

###  Calculate Euclidean distance between all points
grass78  -f -text --tmp-location  -c EPSG:4326 <<'EOF'

#  import points
v.in.ogr --o input=$DIR/ref_points.gpkg layer=ref_points \
output=allpoints type=point key=$SITE

#  Calculate distance, results are given in meters
v.distance -pa from=allpoints to=allpoints upload=dist separator=comma \
    | sed -re 's/([0-9]+\.[0-9]{2})[0-9]+/\1/g' | \
    awk -F, 'NR > 1 {print $0}' > $OUT 

EOF

rm $DIR/ref_points.gpkg

exit



