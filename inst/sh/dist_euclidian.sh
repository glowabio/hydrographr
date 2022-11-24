#! /bin/bash

#export DIR=/home/marquez/hydrographr
#export DAT=spdata_1264942_ids_snap.txt
#export LON=lon_snap
#export LAT=lat_snap

# path - location of input data
export DIR=$1

# name of input table
export DAT=$2

# name of lon and lat coordinates
export LON=$3
export LAT=$4

################################
################################

## save name of file without extension
b=$(echo $DAT | awk -F"." '{print $1}')

## if the file is not csv, add the comma and make it .csv
if [ "${DAT: -4}" != ".csv" ]
then
    cat  $DAT | tr -s '[:blank:]' ',' > ${b}.csv
    export DATC=$(echo ${b}.csv)
else
    DATC=$DAT
fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points.gpkg $DATC -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES

# Name of column for unique ID
export SITE=$( awk 'NR==1 {print $1}' $DAT )

###  Calculate Euclidean distance between all points
grass78  -f -text --tmp-location  -c EPSG:4326 <<'EOF'

#  import points
v.in.ogr --o input=$DIR/ref_points.gpkg layer=ref_points \
output=allpoints type=point key=$SITE

#  Calculate distance, results are given in meters
v.distance -pa from=allpoints to=allpoints upload=dist separator=space \
> $DIR/pairwise_euclidian_dist.txt

EOF

exit


# testing how to make all values two decimal float numbers
awk 'FNR > 2 { for (i=2; i<=NF; i++) a[i]=sprintf("%.2f ", $i) } 
END {print a[2]}' test.txt



