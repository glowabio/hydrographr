#!/bin/bash

export DATADIR=$1
declare -a SUBC_IDS=($2)
export SUBC_LAYER=$3
export VAR=$4
export CALC_ALL=$5
export NO_DATA=$5

export TMPTAX=$DATADIR/tmp
export OUTDIR=$DATADIR/tmp/r_univar


# Check no data value

if [[ $NO_DATA != 'as is' ]] ; then

  gdal_edit.py $DATADIR/$VAR -a_nodata $NO_DATA

fi

echo "$NO_DATA will be used as the no data value in the calculations"

# To silence non-error output, we redirect stdout to /dev/null:
# command > /dev/null

##################################
######     START GRASS
grass78 -f --gtext --tmp-location $SUBC_LAYER  <<'EOF'

  VARNAME=$(basename $VAR .tif)

  # Read in subcatchment raster
  r.in.gdal --o input=$SUBC_LAYER output=subc

  if [ $CALC_ALL == 0 ]; then

    # Procedure to create a subcatchment raster but only with the given subcatchments
    r.reclass --o input=subc output=subc_recl rules=$TMPTAX/reclass_rules.txt
    r.mapcalc --o "subconly = if(subc_recl==1,subc,null())"

    # Rename the reclassified layer
    g.rename raster=subconly,subc --overwrite

  fi

  # Calculate the statistics of the given variable for each microbasin and save in a temporary .csv file
  # Read in variable raster
  r.external  input=$DATADIR/$VAR  output=$VARNAME --overwrite

  # Add header to the output .csv file
  echo "subc_id,${VARNAME}_min,${VARNAME}_max,${VARNAME}_range,${VARNAME}_mean,${VARNAME}_sd" \
       > $OUTDIR/stats_${VARNAME}.csv
  # Calculate zonal statistics and append to the output .csv
  r.univar -t --o map=$VARNAME zones=subc  separator=comma |  awk -F, 'BEGIN{OFS=",";} NR>1 {print $1, $5, $6, $7, $8, $10}'  \
    >> $OUTDIR/stats_${VARNAME}.csv


EOF

