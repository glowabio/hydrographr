#!/bin/sh

export DATA=$1
export LON=$2
export LAT=$3
export LAKE_SHAPE=$4
export COORD=$5
export VAR_ID=$6
export BBOX=$7
export TMPDIR=$8
export OUTDIR=$9

xmin=$(awk -F'[[:space:]]+' 'NR==2 {gsub(/"/, "", $2); print $2}' "$COORD")
ymin=$(awk -F'[[:space:]]+' 'NR==3 {gsub(/"/, "", $2); print $2}' "$COORD")
xmax=$(awk -F'[[:space:]]+' 'NR==4 {gsub(/"/, "", $2); print $2}' "$COORD")
ymax=$(awk -F'[[:space:]]+' 'NR==5 {gsub(/"/, "", $2); print $2}' "$COORD")

export xmin
export ymin
export xmax
export ymax

# add a step if the user uses a shapefile with no ID that generates an ID for each shapefile used in the subsequent steps
if [ "$BBOX" = "TRUE" ]
then
# first case user provides species point ocurrences or bounding box and recieves all lakes within the bounding box
echo "$VAR_ID" > $TMPDIR/lake_id.txt

# ogrinfo $LAKE_SHAPE -al -spat $xmin $ymin $xmax $ymax \
# | grep $VAR_ID | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' \
# | grep -v '^0$' >> $TMPDIR/lake_id.txt # add instead of Hylak_id [VAR_ID] where the user defines the name of the attribute ID column

# this works
ogrinfo $LAKE_SHAPE -al -spat $xmin $ymin $xmax $ymax \
| grep $VAR_ID \
| grep -Eo '[+-]?([1-9][0-9]*([.][0-9]+)?|[0][.][0-9]+)' \
| grep -v '^0\.0$' >> $TMPDIR/lake_id.txt

# tail -n +2 $TMPDIR/lake_id_${RAND_STRING}.txt

# tail -n +2 "$TMPDIR/lake_id_${RAND_STRING}.txt" >> "$TMPDIR/lake_id.tmp" && mv "$TMPDIR/lake_id.tmp" "$TMPDIR/lake_id.txt"

# elif [ "$BBOX" = "FALSE" ] && [ "$LAKE_RAST" = "FALSE" ]; then
cp "$TMPDIR/lake_id.txt" "$OUTDIR/lake_id.txt"
else
  # second case user wants to receive the hydro lake IDs which intersect with their species point ocurrences
  # crop the lake shapefiles to the area of interest provided by the ($xmin $ymin $xmax $ymax) of the occurrence points --> make an array containing $xmin $ymin $xmax $ymax

  ogr2ogr $TMPDIR/lakes.shp \
$LAKE_SHAPE -spat $xmin $ymin $xmax $ymax

export LAKE=$TMPDIR/lakes.shp

# if lake has no id
# if $VAR_ID == NULL
# then
# ogr2ogr -sql "SELECT *, row_number() OVER() as id FROM $LAKE" /data/ttomiczek/lakes_out_inlet_basins/test_muggel.shp $LAKE
# else
export pn=$(basename $LAKE .shp)

# EXTENSION=($( ogrinfo  $LAKE -so -al | grep Extent \
#  grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ))

# temp=${EXTENSION[0]}
# if (($(bc <<< "$temp < 0")))
# then
#    xmin=$(echo $temp | awk '{print int($1)-1}')
# else
#    xmin=$(echo $temp | awk '{print int($1)}')
# fi

# temp=${EXTENSION[2]}
# if (($(bc <<< "$temp < 0")))
# then
#    xmax=$(echo $temp | awk '{print int($1)}')
#else
#    xmax=$(echo $temp | awk '{print int($1)+1}')
# fi

# temp=${EXTENSION[1]}
# if (($(bc <<< "$temp < 0")))
# then
#     ymin=$(echo $temp | awk '{print int($1)-1}')
# else
#     ymin=$(echo $temp | awk '{print int($1)}')
# fi

# temp=${EXTENSION[3]}
# if (($(bc <<< "$temp < 0")))
# then
#     ymax=$(echo $temp | awk '{print int($1)}')
# else
#     ymax=$(echo $temp | awk '{print int($1)+1}')
# fi


## Rasterize lake to use later as the mask layer

# add column as reference for raster creation
ogrinfo $LAKE -sql "ALTER TABLE $pn  ADD COLUMN diss INTEGER"
ogrinfo $LAKE -dialect SQLite -sql "UPDATE $pn SET diss = 1"

gdal_rasterize -a_srs EPSG:4326  -at -a $VAR_ID -l $pn \
-tr 0.000833333333333 -0.000833333333333 \
-te $xmin $ymin $xmax $ymax -a_nodata 0 -ot INT32 \
$LAKE $TMPDIR/lake.tif

LAKE_RAST=$TMPDIR/lake.tif
export LAKE_RAST
# users can also provide a raster .tif file to check if occurrence points fall into them
# export LAKE_RAST=$LAKE_RAST
# move other elif [ "$BBOX" = "FALSE" ] && [ "$LAKE_RAST" = "TRUE" ] here and put fi after export LAKE_RAST lake raster and delete part after line 110 -137
# add header
echo "$VAR_ID" > $TMPDIR/lake_id.txt

awk -v LON=$LON -v LAT=$LAT '
    NR==1 {
        for (i=1; i<=NF; i++) {
            f[$i] = i
        }
    }
    { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
    ' $DATA   | gdallocationinfo -valonly -geoloc  $LAKE_RAST >> $TMPDIR/lake_id.txt

paste -d" " $DATA $TMPDIR/lake_id.txt > $OUTDIR # maybe do this step in R to avoid another value, also call this OUTDIR
fi
# when only a csv file is provided by the user use following code
# # Step 1: Extract column indices for 'long' and 'lat' from the CSV header
# LON=$(awk -F, 'NR==1 {for (i=1; i<=NF; i++) if ($i == "long") print i}' "$DATA")
# LAT=$(awk -F, 'NR==1 {for (i=1; i<=NF; i++) if ($i == "lat") print i}' "$DATA")

# Step 2: Use these indices to print the correct columns and pass to gdallocationinfo
# awk -F, -v LON=$LON -v LAT=$LAT '
#  NR>1 {
#    # Print the longitude and latitude columns
#     print $LON, $LAT
# }
# ' "$DATA" | gdallocationinfo -valonly -geoloc "$LAKE_RAST" >> "$TMPDIR/lake_id_${RAND_STRING}.txt"

# rm $TMPDIR/lake_id_${RAND_STRING}.txt
# exit
# check if I need to put an exit here
# elif [ "$BBOX" = "FALSE" ] && [ "$LAKE_RAST" = "TRUE" ] # lake raster needs not to be logical true/false but is.null or not in bash check gpt
# then
# export LAKE_RAST=$LAKE_RAST

# add header
#  echo "$VAR_ID" > $TMPDIR/lake_id_${RAND_STRING}.txt

#  awk -v LON=$LON -v LAT=$LAT '
# NR==1 {
#    for (i=1; i<=NF; i++) {
#       f[$i] = i
#  }
# }
# { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
#' $DATA   | gdallocationinfo -valonly -geoloc  $LAKE_RAST >> $TMPDIR/lake_id_${RAND_STRING}.txt

# paste -d" " $DATA $TMPDIR/lake_id_${RAND_STRING}.txt > $OUTDIR # maybe do this step in R to avoid another value, also call this OUTDIR
# rm $TMPDIR/lake_id_${RAND_STRING}.txt
# fi
# exit

