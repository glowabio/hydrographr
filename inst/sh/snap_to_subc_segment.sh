#! /bin/bash

##export DATA=spdata_1264942_ids.txt
# testing
#export DATA=spdata_1264942_ids.txt
export DIR=/home/marquez/hydrographr
export DIR=/home/marquez/hydrographr
[ ! -d $DIR/snappoints ] && mkdir $DIR/snappoints
export OUTDIR=$DIR/snappoints
export SITE=$( awk 'NR==1 {print $1}' $DATA )
export BASIN=basin_1264942.tif
export SUBCATCH=subcatchment_1264942.tif
export VECT=order_vect_59.gpkg
export LON=longitude
export LAT=latitude


# input dataset
export DATA=$1

## names of the lon and lat columns
export LON=$2
export LAT=$3

# basin file name
export BASIN=$4

# subcatchment file name
export SUBCATCH=$5

# stream vector gpkg name
export VECT=$6

# How many cores to run in parallel
export PAR=$7

## Full path to output snap_points.txt file
export SNAP=$8

# path to temporary folder
export DIR=$9

# prepare target folder for temporal data
[ ! -d $DIR/snappoints ] && mkdir $DIR/snappoints
export OUTDIR=$DIR/snappoints

###################################
###################################

## Set random string
export RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)

# name of unique id identifier
export SITE=$( awk -F, 'NR==1 {print $1}' $DATA )

### save name of file without extension
#export b=$(echo $DATA | awk -F"." '{print $1}')
#
### if the file is not csv, add the comma and make it .csv
#if [ "${DATA: -4}" != ".csv" ]
#then
#    cat  $DATA | tr -s '[:blank:]' ',' > ${b}.csv
#    export DATAC=$(echo ${b}.csv)
#else
#    DATAC=$DATA
#fi

##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $DIR/ref_points_${RAND_STRING}.gpkg $DATA -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES

################
################

# function to do the snapping per point
SnapPoint(){

# id of the point
export ID=$1

# identify basin ID for that point
export MAB=$(awk -F, -v id="$ID" '$1==id {print $4}' $DATA)
# identify sub-catchment ID for that point
export MIB=$(awk -F, -v id="$ID" '$1==id {print $5}' $DATA)

# extract point of interest
ogr2ogr -where "$SITE = $ID" -f GPKG $DIR/point_${ID}_${RAND_STRING}.gpkg \
    $DIR/ref_points_${RAND_STRING}.gpkg

# extract vector line (stream reach) associated with point
ogr2ogr -nln orderV_bid${MAB} -nlt LINESTRING -where "stream = ${MIB}" \
    -f GPKG $DIR/Microb_${MIB}_${RAND_STRING}.gpkg $VECT

# open grass session based on microbasin raster
grass -f --text --tmp-location $SUBCATCH <<'EOF'

    # read in point of interest
    v.in.ogr input=$DIR/point_${ID}_${RAND_STRING}.gpkg layer=ref_points output=point_$ID \
    type=point key=$SITE

    # read vector line representing stream reach
    v.in.ogr input=$DIR/Microb_${MIB}_${RAND_STRING}.gpkg layer=orderV_bid${MAB} \
        output=streamReach_$MIB type=line key=stream

    # Raster with microbasins
    r.in.gdal input=$SUBCATCH output=micb

    # extract microbasin of stream reach $MIB as raster
    r.mapcalc --o "micr_${ID} = if(micb != ${MIB}, null(), 1)"

    # make the raster a vector points
    r.to.vect --o input=micr_${ID} output=micr_vp_${ID} type=point

    # of how many pixels the raster consist?
    # 1 if stream reach with only one pixel
    # meaning the points already overlap
    NUMP=$(v.info micr_vp_${ID}  | awk '/points/{print $5}')

    if [ $NUMP -eq 1 ]
    then
        v.net --o -s input=streamReach_$MIB  points=point_${ID} \
        output=snap_${ID} operation=connect threshold=90 arc_layer=1 \
        node_layer=2

        v.out.ascii input=snap_${ID} layer=2 separator=comma \
        > ${OUTDIR}/coords_${ID}_${RAND_STRING}

    else

        v.distance -pa from=micr_vp_${ID} to=micr_vp_${ID}  upload=dist \
          > $DIR/dist_mat_p${ID}_${MAB}_${MIB}_${RAND_STRING}.txt

        # calculate maximum distance between all points in microbasin
        MAXDIST=0
        for i in \
        $( seq -s' ' 2 $(v.info micr_vp_${ID}  | awk '/points/{print $5}') )
        do
          newmax=$(awk -F'|' -v X="$i" '{print $X}' \
          $DIR/dist_mat_p${ID}_${MAB}_${MIB}_${RAND_STRING}.txt | sort -n | tail -n1)
          if (( $(echo "$newmax > $MAXDIST" | bc -l) ));then MAXDIST=$newmax;fi
        done

        v.net --o -s input=streamReach_${MIB}  points=point_${ID} \
        output=snap_${ID} operation=connect threshold=$MAXDIST arc_layer=1 \
        node_layer=2

        v.out.ascii input=snap_${ID} layer=2 separator=comma \
        > ${OUTDIR}/coords_${ID}_${RAND_STRING}

        rm $DIR/dist_mat_p${ID}_${MAB}_${MIB}_${RAND_STRING}.txt $DIR/point_${ID}_${RAND_STRING}.gpkg \
        $DIR/Microb_${MIB}_${RAND_STRING}.gpkg
    fi
EOF
}

export -f SnapPoint

IDS=$(awk -F, 'NR > 1 {print $1}' $DATA)
time parallel -j $PAR --delay 5 SnapPoint ::: $IDS


#  Join all single tables in one file
echo lon_snap,lat_snap,Site_ID_snap > ${OUTDIR}/snap_all_${RAND_STRING}.csv
cat ${OUTDIR}/coords_* >> ${OUTDIR}/snap_all_${RAND_STRING}.csv

# Join original table with new coordinates
paste -d" "   \
    $DATA \
    <(sort -t, -k3 -h ${OUTDIR}/snap_all_${RAND_STRING}.csv | awk -F, '{print $1, $2}')  \
    > $SNAP

# remove temporal folder
rm -rf $OUTDIR
rm $DATA
