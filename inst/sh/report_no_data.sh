#!/bin/bash

export DATADIR="$1"
declare VARS=($(echo $2 | tr "/" "\n"))
export NCORES=$3



report_no_data(){

    export VAR=$1

    # Check no data value of the input file
    export NO_DATA=$(gdalinfo "$DATADIR"/$VAR | grep "NoData Value="  | awk -F"=" '{print $2}')

    # report no data value
    echo "$VAR=$NO_DATA"

}

export -f report_no_data

# VARS should be provided as an array from R
parallel -j $NCORES report_no_data ::: ${VARS[*]}
