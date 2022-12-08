#!/bin/bash

export DATADIR=$1
declare -a SUBC_IDS=($2)
export SUBC_LAYER=$3
declare VARS=($4)
export CALC_ALL=$5
export NCORES=$6

export TMPTAX=$DATADIR/tmp
export OUTDIR=$DATADIR/tmp/r_univar


extract_zonal_stat(){

    export VAR=$1

    export VARNAME=$(basename $VAR .tif)

grass78 -f --gtext --tmp-location $SUBC_LAYER   <<'EOF'


    # Read in subcatchment raster
    r.in.gdal --o input=$SUBC_LAYER output=subc --qq

    if [ $CALC_ALL == 0 ]; then

        # Procedure to create a subcatchment raster but only with the given subcatchments
        r.reclass --o input=subc output=subc_recl rules=$TMPTAX/reclass_rules.txt  --qq
        r.mapcalc --o "subconly = if(subc_recl==1,subc,null())"  --qq

        # Rename the reclassified layer
        g.rename raster=subconly,subc --overwrite  --qq

    fi

    # Calculate the statistics of the given variable for each microbasin and save in a temporary .csv file
    # Read in variable raster
    r.external  input=$DATADIR/$VAR  output=$VARNAME --overwrite  --qq

    # Add header to the output .csv file
    echo "subc_id,${VARNAME}_min,${VARNAME}_max,${VARNAME}_range,${VARNAME}_mean,${VARNAME}_sd" \
        > $OUTDIR/stats_${VARNAME}_tmp.csv
    # Calculate zonal statistics and append to the output .csv
    r.univar --qq -t --o map=$VARNAME zones=subc  separator=comma |  awk -F, 'BEGIN{OFS=",";} NR>1 {print $1, $5, $6, $7, $8, $10}'  \
        >> $OUTDIR/stats_${VARNAME}_tmp.csv

EOF

# Sort variables to later join them in R
sort -k 1n $OUTDIR/stats_${VARNAME}_tmp.csv > $OUTDIR/stats_${VARNAME}.csv  && rm -f  $OUTDIR/stats_${VARNAME}_tmp.csv

}


export -f extract_zonal_stat

# VARS should be provided as an array from R
parallel -j $NCORES extract_zonal_stat ::: ${VARS[*]}

