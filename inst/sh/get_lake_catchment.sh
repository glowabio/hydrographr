#!/bin/sh

export DATA=$1
export LAKEID=$2
export DIRECTION=$3
export TMPDIR=$4
export OUTDIR=$5
export NCORES=$6

export LK=$(awk -v colname=$LAKEID '
NR == 1 {
    # Locate the position of the "outlet_ID" column in the header row
    for (i = 1; i <= NF; i++) {
        if ($i == colname) {
            outlet_col = i
            break
        }
    }
    next
}
NR == 2 && outlet_col {
    # Print the value in the "outlet_ID" column for the first data row
    print $outlet_col
}' $DATA)

echo ---------------------------------
echo CREATING UPSTREAM BASINS IN INTERSECTIONS FOR LAKE ${LK} AND OUTLET
echo ---------------------------------

# extention to consider for upstream creation
export EXT=$(awk '{print $2, $3, $4, $5}' $OUTDIR/lakes_ref_extent_${LK}.txt)
export DATACOORD_P=$DATA
echo $DATACOORD_P
tail -n +2 $DATACOORD_P > $TMPDIR/lake_data.txt
export DATACOORD=$TMPDIR/lake_data.txt
echo $DATACOORD

## create lake raster layer with whole extention (add NAs)
gdalwarp  -co COMPRESS=LZW -co ZLEVEL=9 -te $EXT \
    $OUTDIR/lake_${LK}.tif \
    $TMPDIR/lake_${LK}_NA.tif -overwrite

gdalwarp  -co COMPRESS=LZW -co ZLEVEL=9 -te $EXT \
    $DIRECTION $TMPDIR/dir_${LK}_NA.tif -overwrite

grass --tmp-project $TMPDIR/lake_${LK}_NA.tif --exec bash -c "
# Read binary lake layer with same extent as direction
r.external --overwrite input=$TMPDIR/lake_${LK}_NA.tif output=lake

# Read direction map
r.external --overwrite -a input=$TMPDIR/dir_${LK}_NA.tif output=dir

# Define function to calculate sub-basin
BasinCalc() {
    export COD=\"\$1\"

    # Get coordinates from data file
    coords=\$(awk -v coord=\"\$COD\" 'BEGIN {OFS=\",\"} \$1 == coord {print \$3, \$4}' \"\$DATACOORD\")

    # Calculate the sub-basin
    r.water.outlet --overwrite input=dir output=bf_\${COD} coordinates=\${coords}

    # Export the basin as GeoTIFF
    r.out.gdal --overwrite -f -c -m \
        createopt=\"COMPRESS=DEFLATE,ZLEVEL=9\" \
        type=Byte format=GTiff nodata=0 \
        input=bf_\${COD} output=$OUTDIR/basin_lake_${LK}_coord_\${COD}.tif
}

export -f BasinCalc

# Run BasinCalc in parallel for each coordinate ID
awk '{print \$1}' \"\$DATACOORD\" | parallel -j $NCORES BasinCalc
"

COD=$(cat $DATACOORD | awk '{print $1}' $DATACOORD)

# remove temporal files
rm $TMPDIR/lake_${LK}_NA.tif $TMPDIR/dir_${LK}_NA.tif

awk '{gsub(/ /,","); print}' $DATA \
    > $TMPDIR/coord_lake_${LK}.csv

# create gpkg of outlets
# create cvs table of intersections
ogr2ogr -f "GPKG" \
 -oo X_POSSIBLE_NAMES=lon -oo Y_POSSIBLE_NAMES=lat  -oo AUTODETECT_TYPE=YES \
 $OUTDIR/outlets_${LK}.gpkg $TMPDIR/coord_lake_${LK}.csv

exit
