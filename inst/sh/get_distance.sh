#! /bin/bash

# path to temporary destination folder
export OUTDIR="$1"

# full name of input table. Must be a .csv file
export DATA="$2"

# names of lon and lat coordinates and basinID column
export LON=$3
export LAT=$4

   # from here on all parameters are only needed for the network
   # distance calculation
# full path to stream network gpkg file
export STREAM="$5"

# full path to the euclidean distances output file
export DIST_EUCL="$6"

# full path to the network distances output file
export DIST_NET="$7"

# number of cores if running in parallel
export PAR=$8

# which distances to provide ("euclidean", "network", "both")
export DIST=$9


##  make the file a gpkg
ogr2ogr -f "GPKG" -overwrite -nln ref_points -nlt POINT -a_srs EPSG:4326 \
    "$OUTDIR"/ref_points.gpkg "$DATA" -oo X_POSSIBLE_NAMES=$LON \
    -oo Y_POSSIBLE_NAMES=$LAT -oo AUTODETECT_TYPE=YES

# Name of column for unique ID
export SITE=$( awk -F, 'NR==1 {print $1}' "$DATA" )


if [ "$DIST" = euclidean  ] || [ "$DIST" = both  ]

then

  # create table to store output of distance algorithms
  echo "from_$SITE,to_$SITE,dist" > $DIST_EUCL

  #  Calculate Euclidean distance between all points
  grass  -f --gtext --tmp-location  EPSG:4326 <<'EOF'

  #  Import points
  v.in.ascii in="$DATA" out=allpoints separator=comma cat=1 x=2 y=3 skip=1

  #  Calculate distance, results are given in meters
  v.distance -pas from=allpoints to=allpoints upload=dist separator=comma \
      | sed -re 's/([0-9]+\.[0-9]{3})[0-9]+/\1/g' | \
      awk -F, 'NR > 1 {print $0}' > $DIST_EUCL

EOF

fi

if [ "$DIST" = network ] || [ "$DIST" = both  ]
then


# function to do the network distance calculations per basin
# where each basin can be sent to a core in parallel

DistCalc(){

  # create table to store output of distance algorithms
  echo "from_$SITE,to_$SITE,dist" > $DIST_NET

  grass -f --gtext --tmp-location  "$STREAM" <<'EOF'

    # Import points
    # v.in.ascii in="$DATA" out=data_points separator=comma cat=1 x=2 y=3 skip=1

    # read reference points
    v.in.ogr --o input="$OUTDIR"/ref_points.gpkg layer=ref_points output=data_points \
        type=point key=$SITE

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

# Run the function
DistCalc

fi


exit
