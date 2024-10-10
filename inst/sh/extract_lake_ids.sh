#!/bin/sh

export LAKE_SHAPE=$1
export XMIN=$2
export YMIN=$3
export XMAX=$4
export YMAX=$5
export TMPDIR=$6
export VAR_ID=$7
export LON=$8
export LAT=$9
export DATA=${10}
export BBOX=${11}
export OUTDIR=${12}
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)

# add a step if the user uses a shapefile with no ID that generates an ID for
# each shapefile used in the subsequent steps
if [ "$BBOX" == 1 ]; then

echo "bbox used  $BBOX"

echo "LAKE_SHAPE: $LAKE_SHAPE"
echo "XMIN: $XMIN"
echo "YMIN: $YMIN"
echo "XMAX: $XMAX"
echo "YMAX: $YMAX"
echo "TMPDIR: $TMPDIR"
echo "VAR_ID: $VAR_ID"
echo "LON: $LON"
echo "LAT: $LAT"
echo "DATA: $DATA"
echo "BBOX: $BBOX"
echo "OUTDIR: $OUTDIR"

# first case user provides species point ocurrences or bounding box and
# recieves all lakes within the bounding box

echo "$VAR_ID" > $TMPDIR/lake_ids_${RAND_STRING}.txt
echo "created table with header"

# this works
ogrinfo $LAKE_SHAPE -al -spat $XMIN $YMIN $XMAX $YMAX \
| grep $VAR_ID \
| grep -Eo '[+-]?([1-9][0-9]*([.][0-9]+)?|[0][.][0-9]+)' \
| grep -v '^0\.0$' >> $TMPDIR/lake_ids_${RAND_STRING}.txt

cp $TMPDIR/lake_ids_${RAND_STRING}.txt $OUTDIR/lake_ids.txt


elif [ "$BBOX" == 0 ]; then

echo "bbox not used  $BBOX"

ogr2ogr $TMPDIR/lakes.shp \
	     $LAKE_SHAPE -spat $XMIN $YMIN $XMAX $YMAX

echo "created lake shape file"

export LAKE=$TMPDIR/lakes.shp
export pn=$(basename $LAKE .shp)


# add column as reference for raster creation
ogrinfo $LAKE -sql "ALTER TABLE $pn  ADD COLUMN diss INTEGER"
ogrinfo $LAKE -dialect SQLite -sql "UPDATE $pn SET diss = 1"

echo "retrieved lake info"

EXTENSION=($( ogrinfo  $LAKE -so -al | grep Extent \
   | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ))

temp=${EXTENSION[0]}
if (($(bc <<< "$temp < 0")))
then
    XMIN=$(echo $temp | awk '{print int($1)-1}')
 else
     XMIN=$(echo $temp | awk '{print int($1)}')
 fi

 temp=${EXTENSION[2]}
 if (($(bc <<< "$temp < 0")))
 then
     XMAX=$(echo $temp | awk '{print int($1)}')
 else
     XMAX=$(echo $temp | awk '{print int($1)+1}')
 fi

 temp=${EXTENSION[1]}
 if (($(bc <<< "$temp < 0")))
 then
     YMIN=$(echo $temp | awk '{print int($1)-1}')
 else
     YMIN=$(echo $temp | awk '{print int($1)}')
 fi

 temp=${EXTENSION[3]}
 if (($(bc <<< "$temp < 0")))
 then
     YMAX=$(echo $temp | awk '{print int($1)}')
 else
     YMAX=$(echo $temp | awk '{print int($1)+1}')
 fi

 gdal_rasterize -a_srs EPSG:4326  -at -a $VAR_ID -l $pn \
 -tr 0.000833333333333 -0.000833333333333 \
 -te $XMIN $YMIN $XMAX $YMAX -a_nodata 0 -ot Int32 \
 $LAKE $TMPDIR/lake.tif

echo "rasterized lake file"

 LAKE_RAST=$TMPDIR/lake.tif
 export LAKE_RAST
#
echo $DATA

echo "$VAR_ID" > $TMPDIR/lake_ids_${RAND_STRING}.txt
   awk -v LON=$LON -v LAT=$LAT '
        NR==1 {
            for (i=1; i<=NF; i++) {
                f[$i] = i
            }
        }
        { if(NR>1) {print $(f[LON]), $(f[LAT]) }}
        ' $DATA   | gdallocationinfo -valonly -geoloc  $LAKE_RAST >> $TMPDIR/lake_ids_${RAND_STRING}.txt

echo "extracted lake ID"

 paste -d" " $DATA $TMPDIR/lake_ids_${RAND_STRING}.txt > $OUTDIR/lake_ids.txt
fi
rm $TMPDIR/coordinates_* $TMPDIR/lake*

