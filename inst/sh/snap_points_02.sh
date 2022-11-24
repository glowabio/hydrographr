#! /bin/bash

### testing
#export DAT=spdata_1264942_ids.txt
#export DIR=/home/marquez/hydrographr
#export TMP=/home/marquez/hydrographr 
#[ ! -d $TMP/snappoints ] && mkdir $TMP/snappoints
#export OUTDIR=$TMP/snappoints
#export SITE=$( awk 'NR==1 {print $1}' $DAT )
#export BASIN=basin_h18v04.tif
#export SUBCATCH=sub_catchment_h18v04.tif
#export VECT=order_vect_59.gpkg
#export LON=longitude
#export LAT=latitude


# input dataset
export DAT=$1

## names of the lon and lat columns
export LON=$2
export LAT=$3

# path to data inputs
export DIR=$4

# path to temporal folder
export TMP=$5

# prepare target folder for temporal data
[ ! -d $TMP/snappoints ] && mkdir $TMP/snappoints 
export OUTDIR=$TMP/snappoints

# basin file name
export BASIN=$5

# subcatchment file name
export SUBCATCH=$7

# stream vector gpkg name
export VECT=$8

# How many cores to run in parallel
export PAR=$9

###################################
###################################


# name of unique id identifier
export SITE=$( awk 'NR==1 {print $1}' $DAT )

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

################
################

# function to do the snapping per point
SnapPoint(){

# id of the point 
export ID=$1

# identify basin ID for that point
export MAB=$(awk -v id="$ID" '$1==id {print $6}' $DAT)
# identify sub-catchment ID for that point
export MIB=$(awk -v id="$ID" '$1==id {print $5}' $DAT)

# extract point of interest
ogr2ogr -where "$SITE = $ID" -f GPKG $TMP/point_$ID.gpkg \
    $DIR/ref_points.gpkg

# extract vector line (stream reach) associated with point
ogr2ogr -nln orderV_bid${MAB} -nlt LINESTRING -where "stream = ${MIB}" \
    -f GPKG $TMP/Microb_${MIB}.gpkg $VECT

# open grass session based on microbasin raster
grass78 -f -text --tmp-location -c $SUBCATCH <<'EOF'

    # read in point of interest
    v.in.ogr input=$TMP/point_$ID.gpkg layer=ref_points output=point_$ID \
    type=point key=$SITE

    # read vector line representing stream reach
    v.in.ogr input=$TMP/Microb_$MIB.gpkg layer=orderV_bid${MAB} \
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
        > ${OUTDIR}/coords_${ID}
    
    else 

        v.distance -pa from=micr_vp_${ID} to=micr_vp_${ID}  upload=dist \
          > $TMP/dist_mat_p${ID}_${MAB}_${MIB}.txt
          
        # calculate maximum distance between all points in microbasin
        MAXDIST=0
        for i in \
        $( seq -s' ' 2 $(v.info micr_vp_${ID}  | awk '/points/{print $5}') )
        do
          newmax=$(awk -F'|' -v X="$i" '{print $X}' \
          $TMP/dist_mat_p${ID}_${MAB}_${MIB}.txt | sort -n | tail -n1)
          if (( $(echo "$newmax > $MAXDIST" | bc -l) ));then MAXDIST=$newmax;fi
        done

        v.net --o -s input=streamReach_${MIB}  points=point_${ID} \
        output=snap_${ID} operation=connect threshold=$MAXDIST arc_layer=1 \
        node_layer=2

        v.out.ascii input=snap_${ID} layer=2 separator=comma \
        > ${OUTDIR}/coords_${ID}

        rm $TMP/dist_mat_p${ID}_${MAB}_${MIB}.txt $TMP/point_${ID}.gpkg \
        $TMP/Microb_${MIB}.gpkg
    fi 
EOF
}

export -f SnapPoint

IDS=$(awk -F, 'NR > 1 {print $1}' $DATC)
parallel -j $PAR --delay 5 SnapPoint ::: $IDS 


#  Join all single tables in one file
echo lon_snap,lat_snap,Site_ID_snap > ${OUTDIR}/snap_all.csv
cat ${OUTDIR}/coords_* >> ${OUTDIR}/snap_all.csv

# Join original table with new coordinates 
paste -d" "   \
    $DAT \
    <(sort -t, -k3 -h ${OUTDIR}/snap_all.csv | awk -F, '{print $1, $2}')  \
    > $DIR/${b}_snap.txt

# remove temporal folder
rm -rf $OUTDIR
rm $DATC
