#!/bin/sh

export DATA=$1
export BASIN=$2
export SUBC=$3
export OUT=$4

# The $LAYER is a path either to the subcatchment or to the basin id layer

if [ $BASIN != "NA" ] && [ $SUBC == "NA"]
then

 awk '{print $1, $2}' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN >> $OUT

elif [ $BASIN == "NA" ] && [ $SUBC != "NA"]
then
 awk '{print $1, $2}' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC >> $OUT

else
  awk '{print $1, $2}' $DATA   | gdallocationinfo -valonly -geoloc  $BASIN | awk 'BEGIN{ORS=" "} {if(NR==1) {print $1} ' bt.B.1.log >> $OUT
  awk '{print $1, $2}' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC >> $OUT
fi
