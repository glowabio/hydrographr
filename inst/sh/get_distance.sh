#! /bin/bash

# path to temporary destination folder
export OUTDIR=$1

# full name of input table. Must be a .csv file
export DATA=$2

# names of lon and lat coordinates and basinID column
export LON=$3
export LAT=$4

   # from here on all parameters are only needed for the network
   # distance calculation
export BAS_COL=$5

# full path to stream network gpkg file
export STREAM=$6

# # full path and name of basin .tif file
export BASIN=$7

# full path to the euclidean distances output file
export DIST_EUCL=$8

# full path to the network distances output file
export DIST_NET=$9

# number of cores if running in parallel
export PAR=${10}

# which distances to provide ("euclidean", "network", "both")
export DIST=${11}



##  Convert the file of the snapped coordinates to gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    $OUTDIR/ref_points.gpkg $DATA -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES


# Name of column for unique ID
export SITE=$( awk -F, 'NR==1 {print $1}' $DATA )


if [ "$DIST" = euclidean  ] || [ "$DIST" = both  ]

then

  # create table to store output of distance algorithms
  echo "from_$SITE,to_$SITE,dist" > $DIST_EUCL

  #  Calculate Euclidean distance between all points
  grass  -f --gtext --tmp-location  EPSG:4326 <<'EOF'

  #  import points
  v.in.ogr --o input=$OUTDIR/ref_points.gpkg layer=ref_points \
  output=allpoints type=point key=$SITE

  #  Calculate distance, results are given in meters
  v.distance -pas from=allpoints to=allpoints upload=dist separator=comma \
      | sed -re 's/([0-9]+\.[0-9]{3})[0-9]+/\1/g' | \
      awk -F, 'NR > 1 {print $0}' > $DIST_EUCL

EOF

fi

if [ "$DIST" = network ] || [ "$DIST" = both  ]
then

# # array of all ids from basins in the input data
# export basinID=($(awk -F, -v basin_col="$BAS_COL" '
# NR == 1 { for (i=1; i<=NF; i++) {f[$i] = i} }
#  NR > 1 {print $(f[basin_col])}' $DATA | sort | uniq))



# function to do the network distance calculations per basin
# where each basin can be sent to a core in parallel

# DistCalc(){
#
#   # Macro-basin
#   export ID=$1
#
#   # create table to store output of distance algorithms
#   echo "from_$SITE,to_$SITE,dist" > $OUTDIR/dist_net/dist_net_${ID}.csv
#
#   grass -f --gtext --tmp-location  $BASIN <<'EOF'
#
#     # Points available in each basin
#     v.in.ogr --o input=$OUTDIR/ref_points.gpkg layer=ref_points \
#         output=data_points type=point  where="$BAS_COL = ${ID}" key=$SITE
#
#     RANGE=$(v.db.select -c data_points col=$SITE)
#
#     # calculation for all points using the streams (as the fish swims)
#
#     # get layer name
#     export LAYER_NAME=$(ogrinfo -al -so $STREAM | grep "Layer name:" | awk '{print $3}')
#
#     # read the stream network .gpkg file
#     v.in.ogr  --o input=$STREAM \
#         layer=$LAYER_NAME output=stream type=line key=stream
#
#     # Connect points to streams
#     # (threshold does not matter because the snapping was done before)
#     v.net -s --o input=stream points=data_points output=stream_pALL \
#         operation=connect threshold=1 arc_layer=1 node_layer=2
#
#     # calculate distance in the stream network between all pairs
#     v.net.allpairs -g --o input=stream_pALL output=dist_all_${ID} \
#     cats=$(echo $RANGE | awk '{gsub(" ",","); print $0}')
#
#     # add results to table
#     v.report -c map=dist_all_${ID} layer=1 option=length units=meters  \
#        | awk -F',' 'BEGIN{OFS=",";} {gsub(/[|]/, ","); print $2, $3, $5}' \
#        >> $OUTDIR/dist_net/dist_net_${ID}.csv
#
# EOF
#
# }

DistCalc(){


  # create table to store output of distance algorithms
  echo "from_$SITE,to_$SITE,dist" > $DIST_NET

  grass -f --gtext --tmp-location  $STREAM <<'EOF'

    # Points available in each basin
    v.in.ogr --o input=$OUTDIR/ref_points.gpkg layer=ref_points \
        output=data_points type=point  key=$SITE

    RANGE=$(v.db.select -c data_points col=$SITE)

    # calculation for all points using the streams (as the fish swims)

    # get layer name
    export LAYER_NAME=$(ogrinfo -al -so $STREAM | grep "Layer name:" | awk '{print $3}')

    # read the stream network .gpkg file
    v.in.ogr  --o input=$STREAM \
        layer=$LAYER_NAME output=stream type=line key=stream

    # Connect points to streams
    # (threshold does not matter because the snapping was done before)
    v.net -s --o input=stream points=data_points output=stream_pALL \
        operation=connect threshold=1 arc_layer=1 node_layer=2

    # calculate distance in the stream network between all pairs
    v.net.allpairs -g --o input=stream_pALL output=dist_all \
    cats=$(echo $RANGE | awk '{gsub(" ",","); print $0}')

    # add results to table
    v.report -c map=dist_all layer=1 option=length units=meters  \
       | awk -F',' 'BEGIN{OFS=",";} {gsub(/[|]/, ","); print $2, $3, $5}' \
       >> $DIST_NET

EOF

}



export -f DistCalc
# Run the function in parallel
# parallel -j $PAR --delay 5 DistCalc ::: ${basinID[@]}

# Run the function
DistCalc

    # if [ "${#basinID[@]}" -eq 1 ]
    # then
    #
    #   mv $OUTDIR/dist_net/dist_net_${basinID}.csv $DIST_NET
    #
    # else
      # echo "from_$SITE,to_$SITE,dist" > $DIST_NET
      # for FILE in $(find $OUTDIR/dist_net/ -name 'dist_net_*')
      # do
      #     awk 'FNR > 1' $FILE >> $DIST_NET
      # done
    # fi

fi

rm $OUTDIR/ref_points.gpkg


exit
