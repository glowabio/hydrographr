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

### first case user provides species point ocurrences or bounding box and recieves all lakes within the bounding box
if [ "$BBOX" = "TRUE" ]
then

echo "$VAR_ID" > $TMPDIR/lake_id.txt

ogr2ogr -f CSV $TMPDIR/lake_subset.csv $LAKE_SHAPE \
    -spat $xmin $ymin $xmax $ymax \
    -select Hylak_id

tail -n +2 $TMPDIR/lake_subset.csv | cut -d',' -f1 >> $TMPDIR/lake_id.txt

rm $TMPDIR/lake_subset.csv

cp "$TMPDIR/lake_id.txt" "$OUTDIR/lake_id.txt"

else

### second case user wants to receive the hydro lake IDs which intersect with their species point ocurrences
  ogr2ogr $TMPDIR/lakes.shp \
$LAKE_SHAPE -spat $xmin $ymin $xmax $ymax

export LAKE=$TMPDIR/lakes.shp
export pn=$(basename $LAKE .shp)

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

paste -d" " $DATA $TMPDIR/lake_id.txt > $OUTDIR
fi


