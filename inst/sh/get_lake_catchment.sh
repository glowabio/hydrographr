#!/bin/sh


export DATA=$1
# export OUTID=$2
export LAKEID=$2
export DIRECTION=$3 # full path flow dir file export DIRECTION=/mnt/shared/data_from_yale/MERIT_HYDRO/dir_tiles_final20d_1p/all_dir_dis.vrt
export TMPDIR=$4
export OUTDIR=$5
export NCORES=$6

# get_lake_catchment() {

# echo ------------------------------------------------------
# echo SELECT THE $TOP TOP INTERSECTION POINTS FOR LAKE ${LK}
# echo ------------------------------------------------------


# in R create a numbering system when somebody choosen to sort after flow max

# make if condition if all then take all intersection points else take number

# awk 'NR <= $TOP' $TMPDIR/coord_lake_${LK}_tmp.txt | sort -k3,3 > $TMPDIR/coord_lake_${LK}_t.txt
# { head -n 1 $TMPDIR/coord_lake_${LK}_tmp.txt; awk 'NR > 1 && NR <= $TOP' $TMPDIR/coord_lake_${LK}_tmp.txt } > $TMPDIR/coord_lake_${LK}_t.txt

# awk -v top="$TOP" '$NF == top' $DATA > $TMPDIR/coord_lake_${LK}_top${TOP}.txt

# export S=$1
#
# echo $ids
#
# awk -v top="${S}" -v colname=$OUTID '
# NR == 1 {
#     # Find the position of the column named "outlet_ID" in the header row
#     for (i = 1; i <= NF; i++) {
#         if ($i == colname) {
#             outlet_col = i
#             break
#         }
#     }
#     next
# }
# # Only proceed if we have found the column position
# outlet_col && $outlet_col == top
# ' "$DATA" > $TMPDIR/coord_outlet_top${S}.txt

# sort -su -t, -k7 $TMPDIR/coord_lake_${LK}.txt > $TMPDIR/coord_lake_${LK}_t.txt
# mv  $TMPDIR/coord_lake_${LK}_t.txt $TMPDIR/coord_lake_${LK}_top${TOP}.txt
# try to find max
# cut -f1 -d"," $OUTDIR/coord_lake_${LK}.txt | sort -n | tail -1 > $OUTDIR/coord_lake_${LK}_t.txt
# or use the option MAXACCU=$(awk '{print $6}' $DATACOORD | sort -g | tail -1)

# export LK=$(awk -v colname="$LAKEID" '
# NR == 1 {
#     # Locate the position of the "outlet_ID" column in the header row
#     for (i = 1; i <= NF; i++) {
#         if ($i == colname) {
#             outlet_col = i
#             break
#         }
#     }
#     next
# }
# NR == 2 && outlet_col {
#     # Print the value in the "outlet_ID" column for the first data row
#     print $outlet_col
# }' "$DATA")

# export LK=$(awk -v col="$LAKEID" '
# NR == 1 {
#   for (i=1; i<=NF; i++) if ($i == col) { colnum = i; break }
#   next
# }
# NR == 2 {
#   print $colnum
#   exit
# }
# ' "$DATA")

# read LK <<< $(awk -v lake="$LAKEID" '
# NR == 1 {
#     for (i = 1; i <= NF; i++) {
#         if ($i == lake) lake_col = i
#     }
#     next
# }
# NR == 2 {
#     print $lake_col
# }' "$DATA")
#
# echo "Lake ID: $LK"

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
# export EXT=$(awk '{print $1, $2, $3, $4}' $tmp/lakes_ref_extent_${LK}.txt)
#export EXT=$(pkinfo -i ${SUPPL}/ALLbasins_${LK}.tif  -te | awk '{print $2,$3,$4,$5}')

# export DATACOORD=$OUTDIR/coord_lake_${LK}_top${TOP}.txt
#
# #Identify the id of the point with the maximum accumulation value (i.e. the outlet)
# MAXACCU=$(awk '{print $6}' $DATACOORD | sort -g | tail -1)
# export OUTLET=$(awk -v outlet=$MAXACCU '$6 == outlet {print $1}' $DATACOORD)
# export DATACOORD=$TMPDIR/coord_outlet_top${S}.txt

export DATACOORD_P=$DATA
echo $DATACOORD_P
tail -n +2 $DATACOORD_P > $TMPDIR/lake_data.txt
export DATACOORD=$TMPDIR/lake_data.txt
echo $DATACOORD
# export DATACOORD=$DATA
# export OUTLET=$OUTDIR/coord_lake_${LK}_top${TOP}.txt

## create lake raster layer with whole extention (add NAs)
gdalwarp  -co COMPRESS=LZW -co ZLEVEL=9 -te $EXT \
    $OUTDIR/lake_${LK}.tif \
    $TMPDIR/lake_${LK}_NA.tif -overwrite

gdalwarp  -co COMPRESS=LZW -co ZLEVEL=9 -te $EXT \
    $DIRECTION $TMPDIR/dir_${LK}_NA.tif -overwrite

# module purge
# module load GRASS/8.2.0-foss-2022b
# module load GDAL/3.6.2-foss-2022b

# LK=$LK

# grass  -f --text --tmp-location $TMPDIR/lake_${LK}_NA.tif <<EOF_SCRIPT
#
# # read binary lake layer with same extent as direction
# r.external --o input=$TMPDIR/lake_${LK}_NA.tif  output=lake
#
# #  read direction map
# r.external --o -a input=$TMPDIR/dir_${LK}_NA.tif output=dir
#
# BasinCalc(){
#
# export COD=\${1}
#
# ## calculate the sub-basin
# r.water.outlet --overwrite input=dir output=bf_\${COD} \
# coordinates=\$(cat \$DATACOORD | awk -v coord=\${COD} 'BEGIN{OFS=",";} \$1==coord {print \$3,\$4}')
# ##  Export the basin as tif file
# r.out.gdal --o -f -c -m  createopt="COMPRESS=DEFLATE,ZLEVEL=9" \
# type=Byte  format=GTiff nodata=0 \
# input=bf_\${COD} output=$OUTDIR/basin_lake_${LK}_coord_\${COD}.tif
#
# }
#
# export -f BasinCalc
# awk '{print \$1}' \$DATACOORD | parallel -j $NCORES BasinCalc
# EOF_SCRIPT

# grass  --tmp-project $TMPDIR/lake_${LK}_NA.tif --exec bash -c "
# # read binary lake layer with same extent as direction
# r.external --o input=$TMPDIR/lake_${LK}_NA.tif  output=lake
#
# #  read direction map
# r.external --o -a input=$TMPDIR/dir_${LK}_NA.tif output=dir
#
# BasinCalc(){
#
# export COD=\${1}
#
# ## calculate the sub-basin
# r.water.outlet --overwrite input=dir output=bf_\${COD} \
# coordinates=\$(cat \$DATACOORD | awk -v coord=\${COD} 'BEGIN{OFS=",";} \$1==coord {print \$3,\$4}')
# ##  Export the basin as tif file
# r.out.gdal --o -f -c -m  createopt="COMPRESS=DEFLATE,ZLEVEL=9" \
# type=Byte  format=GTiff nodata=0 \
# input=bf_\${COD} output=$OUTDIR/basin_lake_${LK}_coord_\${COD}.tif
#
# }
#
# export -f BasinCalc
# awk '{print \$1}' \$DATACOORD | parallel -j $NCORES BasinCalc
# "

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

#-----------------------------

# mkdir  $TMPDIR/upstreamBasins
# mv $TMPDIR/basin_lake_${LK}_coord_${COD}.tif $TMPDIR/upstreamBasins

# }


# export -f get_lake_catchment

# ids=($(awk -v id_name="$OUTID" '
# NR == 1 {
#     # Find the position of the column named by id_name in the header row
#     for (i = 1; i <= NF; i++) {
#         if ($i == id_name) {
#             col = i
#             break
#         }
#     }
# }
# NR > 1 && col {
#     # Print the value in the identified column for each data row
#     print $col
# }' "$DATA"))
# parallel -j $NCORES --delay 3 get_lake_catchment ::: $ids

### Convert table of intersections to csv
# cat $DATA | awk 'NR == 1 { print "outlet_ID lake_ID lon lat subc_id flow_accu flow_accu_max flow_accu_mean" }; 1' \
#   > $TMPDIR/coord_lake_${LK}_p.txt
awk '{gsub(/ /,","); print}' $DATA \
    > $TMPDIR/coord_lake_${LK}.csv

# create gpkg of outlets
# create cvs table of intersections
ogr2ogr -f "GPKG" \
 -oo X_POSSIBLE_NAMES=lon -oo Y_POSSIBLE_NAMES=lat  -oo AUTODETECT_TYPE=YES \
 $OUTDIR/outlets_${LK}.gpkg $TMPDIR/coord_lake_${LK}.csv

# rm $TMPDIR/coord_lake_${LK}.csv

exit
