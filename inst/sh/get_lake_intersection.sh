#!/bin/sh

# path to hydrolake shapefiles export LAKE_SHAPE=/mnt/shared/data_from_yale/dataproces/HydroLAKES/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp
# path to global file of basins export GLBASINS=/mnt/shared/data_from_yale/MERIT_HYDRO/lbasin_tiles_final20d_ovr/all_lbasin.tif
# path to global basin of computational units export GLCOMPUNITS=/mnt/shared/data_from_yale/MERIT_HYDRO/lbasin_compUnit_overview/lbasin_compUnit.tif
# path to basins in each computational unit export CUBASINS=/mnt/shared/data_from_yale/MERIT_HYDRO/CompUnit_lbasin
# path to flow.tif file export FLOW=/mnt/shared/data_from_yale/MERIT_HYDRO/flow_tiles/all_tif_dis.vrt
# path to stream.tif file export STREAM=/mnt/shared/data_from_yale/MERIT_HYDRO/stream_tiles_final20d_1p/all_stream_dis.vrt
# SHP_NAME="HydroLAKES_polys_v10"
# path to GWB analysis tool export GWB=/data/ttomiczek/GWB/

export DATA=$1
export VAR_ID=$2 # maybe dont need this variable when in the R script every data table is rearranged that ID column is the first column??
export LAKE_SHAPE=$3
export SHP_NAME=$4
export BUF=$5
export GWB=$6 # full path to GWB MSPA analysis tool
export STREAM=$7 # full path to stream.tif file
export FLOW=$8 # full path to flow.tif file
export GLBASINS=$9 # full path to global file of basins
export TMPDIR=${10}
export OUTDIR=${11}
export NCORES=${12}


# if gpkg geo data base do get SHP_NAME for HydroLAKES SHP_NAME is HydroLAKES_polys_v10
# ogrinfo $LAKE_SHAPE -so
# if input is a shape file do get  no does not support dataformat use ogrinfo
# gdalinfo $LAKE_SHAPE

# read Hydrolakes ids into an array to process the script in parallel
# LK=($(awk '{print $1}' $DATA))
# get array without the first line of the header otherwise alter the data table in R before sending it to the sh script
# LK=($(awk 'NR > 1 && $1 ~ /^[0-9]+([.][0-9]+)?$/' "$DATA"))

# export LK=$LK
# echo $LK

get_lake_intersection() {
export LK=$1

SECONDS=0
echo "LAKE $LK is being processed"
# SHP_NAME="HydroLAKES_polys_v10"
# try this to run in parallel
# parallel ogr2ogr $TMPDIR/lake_{1}.shp $LAKE_SHAPE -sql "SELECT * FROM $SHP_NAME WHERE $VAR_ID = {1}" ::: "${LK[@]}"
# if gpkg file use the following
ogr2ogr $TMPDIR/lake_${LK}.shp \
	     $LAKE_SHAPE \
       -sql "SELECT * FROM $SHP_NAME WHERE $VAR_ID = ${LK}"

# if its a shapefile use the following
ogr2ogr -where "$VAR_ID = $LK" $TMPDIR/lake_${LK}.shp $LAKE_SHAPE
# ogr2ogr -f "ESRI Shapefile" -sql "SELECT * FROM $SHP_NAME WHERE $VAR_ID = ${LK}" $TMPDIR/lake_${LK}.shp $LAKE_SHAPE
# this name of the shape file HydroLAKES_polys_v10 needs to be defined before either by the user or automatically
        export LAKE=$TMPDIR/lake_${LK}.shp

    ##  calculate the area to define the buffer distance

    # check basename of polygon file
    export pn=$(basename $LAKE .shp)

    area=$(ogrinfo -dialect SQLite -sql \
        "SELECT (ST_Area(ST_Transform(Geometry,3395)))/1000000 FROM ${pn}" \
        $LAKE | awk '/=/{print $4}')
 if [ "$BUF" = "TRUE" ]
  then
 # rule to define buffer distance; bd= units are map units in degree (0.0054 = 600 meters; 0.0009 = 100 meters)
    if (( $(echo "${area} <= 1" | bc -l) )); then export bd=0.0009; fi
    if (( $(echo "${area} > 1" | bc -l) && $(echo "${area} <= 50" | bc -l) )); then export bd=0.0018; fi
    if (( $(echo "${area} > 50" | bc -l) && $(echo "${area} <= 100" | bc -l) )); then export bd=0.0027; fi
    if (( $(echo "${area} > 100" | bc -l) )); then export bd=0.0036; fi

    ogr2ogr -dialect sqlite -sql "SELECT ST_Buffer(Geometry, ${bd}) FROM ${pn}" \
        $TMPDIR/buffer_${LK}.shp \
        $TMPDIR/lake_${LK}.shp

        export LAKE=$TMPDIR/buffer_${LK}.shp

    echo "The buffer size is $bd"

  elif [[ "$BUF" =~ ^[0-9]+([.][0-9]+)?$ ]]; ### example use correct code line
  then
  export bd=$BUF
  ## calculate buffer
    ogr2ogr -dialect sqlite -sql "SELECT ST_Buffer(Geometry, ${bd}) FROM ${pn}" \
        $TMPDIR/buffer_${LK}.shp \
        $TMPDIR/lake_${LK}.shp
    export LAKE=$TMPDIR/buffer_${LK}.shp

  elif [ "$BUF" = "FALSE" ]
  then
   echo "No buffer applied"
   export LAKE=$TMPDIR/lake_${LK}.shp
 fi

# export LAKE=$TMPDIR/buffer_${LK}.shp
export pn=$(basename $LAKE .shp)

EXTENSION=($( ogrinfo  $LAKE -so -al | grep Extent \
    | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ))

temp=${EXTENSION[0]}
if (($(bc <<< "$temp < 0")))
then
    xmin=$(echo $temp | awk '{print int($1)-1}')
else
    xmin=$(echo $temp | awk '{print int($1)}')
fi

temp=${EXTENSION[2]}
if (($(bc <<< "$temp < 0")))
then
    xmax=$(echo $temp | awk '{print int($1)}')
else
    xmax=$(echo $temp | awk '{print int($1)+1}')
fi

temp=${EXTENSION[1]}
if (($(bc <<< "$temp < 0")))
then
    ymin=$(echo $temp | awk '{print int($1)-1}')
else
    ymin=$(echo $temp | awk '{print int($1)}')
fi

temp=${EXTENSION[3]}
if (($(bc <<< "$temp < 0")))
then
    ymax=$(echo $temp | awk '{print int($1)}')
else
    ymax=$(echo $temp | awk '{print int($1)+1}')
fi

ogrinfo $LAKE -sql "ALTER TABLE $pn  ADD COLUMN diss INTEGER"
ogrinfo $LAKE -dialect SQLite -sql "UPDATE $pn SET diss = 1"

gdal_rasterize -a_srs EPSG:4326  -at -a diss -l $pn \
    -tr 0.000833333333333 -0.000833333333333 \
    -te $xmin $ymin $xmax $ymax -a_nodata 0  \
    -co COMPRESS=DEFLATE -co ZLEVEL=9 -ot Byte \
    $LAKE $TMPDIR/lake_${LK}cp.tif

export TMPDIR=$TMPDIR
export LK=$LK

# grass -f --text --tmp-location $TMPDIR/lake_${LK}cp.tif <<EOF
#     r.external -o input=$TMPDIR/lake_${LK}cp.tif  output=out
#     g.region zoom=out
#     r.out.gdal -cm input=out out=$TMPDIR/lake_${LK}rm.tif \
#     format=GTiff type=Byte createopt="COMPRESS=DEFLATE" nodata=0 --overwrite
# EOF
# Maybe try out this code if lake_{$LK}cp.tif does not exist
grass --tmp-project $TMPDIR/lake_${LK}cp.tif --exec bash -c "
    r.external -o input=$TMPDIR/lake_${LK}cp.tif output=out &&
    g.region zoom=out &&
    r.out.gdal -cm input=out out=$TMPDIR/lake_${LK}rm.tif \
        format=GTiff type=Byte createopt='COMPRESS=DEFLATE' nodata=0 --overwrite
"

xmin=$(pkinfo -i $TMPDIR/lake_${LK}rm.tif -te | awk '{print $2-0.001666667}')
ymin=$(pkinfo -i $TMPDIR/lake_${LK}rm.tif -te | awk '{print $3-0.001666667}')
xmax=$(pkinfo -i $TMPDIR/lake_${LK}rm.tif -te | awk '{print $4+0.001666667}')
ymax=$(pkinfo -i $TMPDIR/lake_${LK}rm.tif -te | awk '{print $5+0.001666667}')

gdalwarp -te $xmin $ymin $xmax $ymax -tr 0.000833333333333 -0.000833333333333 \
    -co COMPRESS=DEFLATE -co ZLEVEL=9 -ot Byte \
    $TMPDIR/lake_${LK}rm.tif $TMPDIR/lake_${LK}.tif

cp $TMPDIR/lake_${LK}.tif $OUTDIR/lake_${LK}.tif

# crop basin raster file to the lake extention
gdal_translate \
    -projwin $( pkinfo -i $TMPDIR/lake_${LK}.tif -bb | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ) \
    -co COMPRESS=LZW -co ZLEVEL=9 $GLBASINS $TMPDIR/extentLB_${LK}.tif

### how does the user get the $GLBASINS .tif file? download it before through download function croping allbasin.tif to the extent of interest? ###

# crop compunit raster file to the lake extention
# gdal_translate \
  #  -projwin $( pkinfo -i $TMPDIR/lake_${LK}.tif -bb | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ) \
  # -co COMPRESS=LZW -co ZLEVEL=9 $GLCOMPUNITS $TMPDIR/extentCU_${LK}.tif

### how does the user get the $GLCOMPUNITS .tif file? download it before through download function? ###

# mask the basins raster to the lake boundaries to identify the basin IDs
# that fully overlap with lake
pksetmask -i $TMPDIR/extentLB_${LK}.tif \
    -m $TMPDIR/lake_${LK}.tif -msknodata 0 -nodata 0 \
    -o $TMPDIR/maskLB_${LK}.tif -co COMPRESS=LZW -co ZLEVEL=9

### should there be an option to save lake.tif file to disk for the user? ###

# identify basin IDs
echo "$LK $(pkstat -i $TMPDIR/maskLB_${LK}.tif -hist \
    | awk '$2 > 0 && $1 > 0 {print $1}' | sed -z 's/\n/ /g')" \
    > $TMPDIR/ref_lbasinIDs_${LK}.txt

# mask the compunit raster to the lake boundaries to identify the compUnit IDs
# that fully overlap with lake
# pksetmask -i $TMPDIR/extentCU_${LK}.tif \
  #  -m $TMPDIR/lake_${LK}.tif -msknodata 0 -nodata 0 \
  # -o $TMPDIR/maskCU_${LK}.tif -co COMPRESS=LZW -co ZLEVEL=9

# identify CompUnit IDs
# echo "$LK $(pkstat -i $TMPDIR/maskCU_${LK}.tif -hist \
  #  | awk '$2 > 0 && $1 > 0 {print $1}' | sed -z 's/\n/ /g')" \
  # > $TMPDIR/ref_CompUnitIDs_${LK}.txt

# remove tmp files
# rm $TMPDIR/maskCU_${LK}.tif $TMPDIR/maskLB_${LK}.tif \
# $TMPDIR/extentCU_${LK}.tif $TMPDIR/extentLB_${LK}.tif \
# $TMPDIR/lake_${LK}rm.tif $TMPDIR/lake_${LK}cp.tif

    # get the IDs of the COmpUnits
    CUID=($(cut -d" " -f2- $TMPDIR/ref_lbasinIDs_${LK}.txt))

    # Join Computational Units as VRT
    # create file with list of files to join
    # for i in ${CUID[@]}; do find $GLBASINS -name "lbasin_${i}_msk.tif"; done \
      #  > $TMPDIR/my_list_${LK}.txt
    # create the vrt
    # gdalbuildvrt $TMPDIR/CompUnits_${LK}.vrt \
      # -input_file_list $TMPDIR/my_list_${LK}.txt

    rm $TMPDIR/my_list_${LK}.txt

    # make list of lbasins IDs as an arrray
    export  MBID=( $(cut -d" " -f2- $TMPDIR/ref_lbasinIDs_${LK}.txt) )

   # reclassify raster with lbasins of interest (1) else is (0)
    # identify max value (lbasin ID)
    MAX=$(gdalinfo -mm $GLBASINS | grep Computed \
        | awk -F, '{print $2}')
    # create classification table
    col1=($(seq 0 ${MAX%.*}))
    col2=($(printf '0%.0s\n' $(eval "echo {0.."$((${MAX%.*}))"}" )))
    for i in ${MBID[@]}; do col2[$i]=1; done
    paste -d " " \
        <(printf "%s\n" ${col1[@]:1:${#col1[@]}}) \
        <(printf "%s\n" ${col2[@]:1:${#col2[@]}}) \
        > $TMPDIR/reclass_code_${LK}.txt

    # run reclassification
    pkreclass -i $GLBASINS \
        -o $TMPDIR/lbasin_reclass_${LK}.vrt \
        --code $TMPDIR/reclass_code_${LK}.txt

    rm $TMPDIR/reclass_code_${LK}.txt

# export LK=$LK

# grass -f --text --tmp-location $TMPDIR/lbasin_reclass_${LK}.vrt <<EOF_SCRIPT
#     r.external -o input=$TMPDIR/lbasin_reclass_${LK}.vrt output=out
#     g.region zoom=out
#     g.region -p
#     r.out.gdal input=out out=${TMPDIR}/ALLbasins_${LK}.tif format=GTiff type=Byte createopt="COMPRESS=DEFLATE,BIGTIFF=YES" --overwrite
#     region_info=\$(g.region -w | awk -F '[=,]' '{print \$2,\$3,\$4,\$5}')
#     echo "\$LK \$region_info" > $TMPDIR/lakes_ref_extent_${LK}.txt
# EOF_SCRIPT

grass --tmp-project $TMPDIR/lbasin_reclass_${LK}.vrt --exec bash -c "
    r.external -o input=$TMPDIR/lbasin_reclass_${LK}.vrt output=out &&
    g.region zoom=out &&
    g.region -p &&
    r.out.gdal input=out out=${TMPDIR}/ALLbasins_${LK}.tif \
        format=GTiff type=Byte createopt='COMPRESS=DEFLATE,BIGTIFF=YES' --overwrite &&
    region_info=\$(g.region -w | awk -F '[=,]' '{print \$2,\$3,\$4,\$5}') &&
    echo \"${LK} \$region_info\" > $TMPDIR/lakes_ref_extent_${LK}.txt
"

# extention to consider for upstream creation
export EXT=$(awk '{print $2, $3, $4, $5}' $TMPDIR/lakes_ref_extent_${LK}.txt)

### add an option to use raster files here instead of lake shape files ###
# tell user to have under $GWB the folder $GWB/input and $GWB/output

# mkdir -p $GWB/{in,out}put # check if sudo mkdir is necessary?

pkreclass -co COMPRESS=LZW -co ZLEVEL=9 -ot Byte -of GTiff \
        -i $TMPDIR/lake_${LK}.tif \
        -o $GWB/input/lake_${LK}.tif \
        -c 1 -r 2 -c 0 -r 1

echo "START MSPA"
# make sure $GWB/output is empty to work
rm -rf $GWB/output/*
cp $TMPDIR/lakes_ref_extent_${LK}.txt $OUTDIR/lakes_ref_extent_${LK}.txt
# create mspa-parameters.txt before in R (check if its always the same structure)
cp $TMPDIR/mspa-parameters.txt "$GWB/input" # this file needs to be created in R!
# cat $GWB/input/mspa-parameters.txt
GWB_MSPA -i=$GWB/input -o=$GWB/output
cp $GWB/output/lake_${LK}_mspa/lake_${LK}_8_1_0_1.tif $TMPDIR
rm $GWB/output/mspa.log

echo "MSPA is finished"

# rm -rf $DIR/GWB/{in,out}put # if these folders were created wiht mkdir then remove them afterwards

### reclassification of MSPA output to retain only the outer borders
pkreclass -co COMPRESS=LZW -co ZLEVEL=9 \
    -i $TMPDIR/lake_${LK}_8_1_0_1.tif \
    -o $TMPDIR/mspa_${LK}.tif --code $TMPDIR/mspa_reclass_code.txt

### create mspa_reclass_code.txt in R (check if its is always the same) ###

# warp to avoid pixel missmatch
gdalwarp -tr 0.000833333333333 0.000833333333333 -tap $TMPDIR/mspa_${LK}.tif $TMPDIR/mspa_${LK}_l.tif -overwrite

# crop streams to the extension of interest
gdal_translate -co COMPRESS=LZW -co ZLEVEL=9 \
    -projwin $( pkinfo -i $TMPDIR/mspa_${LK}_l.tif -bb | grep  -Eo '[+-]?[0-9]+([.][0-9]+)?' ) \
    -tr 0.000833333333333 0.000833333333333 $STREAM $TMPDIR/stream_${LK}.tif

###  Multiply to identify intersections
gdal_calc.py -A $TMPDIR/stream_${LK}.tif -B $TMPDIR/mspa_${LK}_l.tif \
    --outfile=$TMPDIR/overlapping_${LK}.tif --calc="A*B" \
    --NoDataValue=0 --overwrite

### crop flow to extension of interest
gdal_translate -co COMPRESS=LZW -co ZLEVEL=9 \
    -projwin $( pkinfo -i $TMPDIR/mspa_${LK}_l.tif -bb | grep  -Eo '[+-]?[0-9]+([.][0-9]+)?' ) \
    $FLOW $TMPDIR/flow_${LK}.tif

# rm $TMPDIR/mspa_${LK}.tif
# cp $TMPDIR/mspa_${LK}_l.tif $TMPDIR/mspa_${LK}.tif
# rm $TMPDIR/mspa_${LK}_l.tif
# rm $GWB/input/lake_${LK}.tif


# grass  -f --text --tmp-location $TMPDIR/overlapping_${LK}.tif <<EOF
# r.external -o input=$TMPDIR/overlapping_${LK}.tif  output=out
# r.external -o input=$TMPDIR/flow_${LK}.tif output=flow
# r.neighbors input=flow output=flow_max method=maximum
# r.neighbors input=flow output=flow_ave method=average
#
# r.mapcalc --o "flowInt = if(isnull(out), null() , flow)"
# r.mapcalc --o "flowMax = if(isnull(out), null() , flow_max)"
# r.mapcalc --o "flowAve = if(isnull(out), null() , flow_ave)"
#
# r.out.xyz --overwrite input=out output=$TMPDIR/coord_lake_${LK}.txt separator=space
# r.out.xyz --overwrite input=flowInt output=$TMPDIR/coord_flowInt_${LK}.txt separator=space
# r.out.xyz --overwrite input=flowMax output=$TMPDIR/coord_flowMax_${LK}.txt separator=space
# r.out.xyz --overwrite input=flowAve output=$TMPDIR/coord_flowAve_${LK}.txt separator=space
# EOF
grass --tmp-project "$TMPDIR/overlapping_${LK}.tif" --exec bash -c "
    r.external -o input='$TMPDIR/overlapping_${LK}.tif' output=out
    r.external -o input='$TMPDIR/flow_${LK}.tif' output=flow
    r.neighbors input=flow output=flow_max method=maximum
    r.neighbors input=flow output=flow_ave method=average

    r.mapcalc --o 'flowInt = if(isnull(out), null(), flow)'
    r.mapcalc --o 'flowMax = if(isnull(out), null(), flow_max)'
    r.mapcalc --o 'flowAve = if(isnull(out), null(), flow_ave)'

    r.out.xyz --overwrite input=out      output='$TMPDIR/coord_lake_${LK}.txt'     separator=space
    r.out.xyz --overwrite input=flowInt  output='$TMPDIR/coord_flowInt_${LK}.txt'  separator=space
    r.out.xyz --overwrite input=flowMax  output='$TMPDIR/coord_flowMax_${LK}.txt'  separator=space
    r.out.xyz --overwrite input=flowAve  output='$TMPDIR/coord_flowAve_${LK}.txt'  separator=space
"
paste -d " " <(seq 1 $(wc -l < $TMPDIR/coord_lake_${LK}.txt))  \
    $TMPDIR/coord_lake_${LK}.txt \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowInt_${LK}.txt)   \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowMax_${LK}.txt)   \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowAve_${LK}.txt)   \
    > $TMPDIR/coord_lake_${LK}_p.txt

paste -d " " <(seq 1 $(wc -l < $TMPDIR/coord_lake_${LK}.txt))  \
    $TMPDIR/coord_lake_${LK}.txt \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowInt_${LK}.txt)   \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowMax_${LK}.txt)   \
    <(awk '{printf "%.3f\n", $3}' $TMPDIR/coord_flowAve_${LK}.txt)   \
    > $OUTDIR/coord_lake_${LK}.txt

rm $TMPDIR/coord_lake_${LK}.txt $TMPDIR/coord_flowInt_${LK}.txt
rm $TMPDIR/coord_flowMax_${LK}.txt $TMPDIR/coord_flowAve_${LK}.txt
# rm $TMPDIR/overlapping_${LK}.tif
# rm $GWB/input/mspa-parameters.txt

# Identify streamIDs that have more than one duplicate
DUP=$(awk '{print $4}' $TMPDIR/coord_lake_${LK}_p.txt | sort | uniq -c | awk '$1 > 1 {print $2}')
echo "DO the looping"
start_time=$(date +%s.%N)
# Loop to go through each of the stream IDs and identify the coordIDs of the duplicates with the lowest flow accumulation. The purpose is to remove those from the main table and leave only one streamID with the highest accumulation value (using as comparison the Maxumin flow $6)
###  Procedure to retain the intersection with the minimum accumulation value (for the maximum replace tail with head)
RM=$(for LINE in $DUP
do
awk -v line=$LINE '$4 == line {print $1, $6}' $TMPDIR/coord_lake_${LK}_p.txt \
    | LC_ALL=C sort -k2 -g \
    | head -n $(echo "$(awk -v line=$LINE '$4 == line' $TMPDIR/coord_lake_${LK}_p.txt | wc -l)" - 1 | bc ) \
    | awk '{print $1}'
done)

# Move through each coordID duplicate and remove it from the main table
for ROW in $RM
do
awk -i inplace -v row=$ROW '$1 == row {next} {print}' $TMPDIR/coord_lake_${LK}_p.txt
done
end_time=$(date +%s.%N)

elapsed_time=$(echo "$end_time - $start_time" | bc)

# Convert to minutes and seconds
minutes=$(echo "$elapsed_time / 60" | bc)
seconds=$(echo "$elapsed_time - ($minutes * 60)" | bc)

echo "Time taken: ${minutes} minutes and ${seconds} seconds"
## add a line of code which creates a new column giving every row a new id from 1 to last row or check in further script if this is already done and add here
echo "Start sorting"
sort -k7 -n -r $TMPDIR/coord_lake_${LK}_p.txt > $TMPDIR/coord_lake_${LK}_sort.txt
paste -d " " <(seq 1 $(wc -l < $TMPDIR/coord_lake_${LK}_sort.txt))  \
  $TMPDIR/coord_lake_${LK}_sort.txt \
  > $TMPDIR/coord_lake_${LK}_tmp.txt

awk -v var=$LK '$(NF+1) = var' $TMPDIR/coord_lake_${LK}_tmp.txt \
> $TMPDIR/coord_lake_${LK}_pro.txt

awk '{print $1,$9,$3,$4,$5,$6,$7,$8}' "$TMPDIR/coord_lake_${LK}_pro.txt" \
> $OUTDIR/coord_lake_${LK}_tmp.txt

# Calculate minutes and seconds
minutes=$(( SECONDS / 60 ))
seconds=$(( SECONDS % 60 ))

echo "Time taken: ${minutes} minutes and ${seconds} seconds"

# rm $TMPDIR/coord_lake_${LK}_pro.txt
# rm $TMPDIR/coord_lake_${LK}_tmp.txt

### check if the order is correct ###
# rearrange the table to have the following structure outlet ID; lake ID; lon; lat; sub_catchment_id; flow accu at pixel; flow accu max; flow accu mean
headers="outlet_ID lake_ID lon lat subc_id flow_accu flow_accu_max flow_accu_mean"

# Write headers to the output file
echo "$headers" > "$OUTDIR/coord_lake_${LK}.txt"

# Append the contents of the .txt file to the output file
cat "$OUTDIR/coord_lake_${LK}_tmp.txt" >> "$OUTDIR/coord_lake_${LK}.txt"

rm $TMPDIR/ALLbasins_${LK}.tif
rm $TMPDIR/lbasin_reclass_${LK}.vrt
rm $TMPDIR/coord_lake_${LK}*
rm $TMPDIR/buffer_${LK}*
rm $TMPDIR/flow_${LK}.tif
# rm $TMPDIR/overlapping_${LK}.tif
rm $TMPDIR/CompUnits_${LK}.vrt
rm $TMPDIR/mspa_reclass_code.txt
rm $TMPDIR/mspa_${LK}*
rm $TMPDIR/lake_${LK}*
rm $TMPDIR/stream_${LK}.tif
rm $TMPDIR/lakes_ref_extent_${LK}.txt
rm $TMPDIR/ref_lbasinIDs_${LK}.txt
rm $TMPDIR/ref_CompUnitIDs_${LK}.txt
rm $GWB/input/lake_${LK}.tif
rm -rf $GWB/output/lake_${LK}_mspa*
rm $OUTDIR/coord_lake_${LK}_tmp.txt

echo "Finished with lake $LK"
echo "Time taken: ${minutes} minutes and ${seconds} seconds"

}

export -f get_lake_intersection

# for LK in "${LK[@]}"; do
#   get_lake_intersection "$LK" &
# done

# wait

# LK should be provided as an array from R
# parallel -j $NCORES get_lake_intersection ::: "${LK[@]}"

ids=$(awk -v id_name=${VAR_ID}  'NR == 1 { for (i=1; i<=NF; i++) {f[$i] = i} } \
    NR > 1 {print $(f[id_name])}' $DATA)
parallel -j $NCORES --delay 3 get_lake_intersection ::: $ids

exit
