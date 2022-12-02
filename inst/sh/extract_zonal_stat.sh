#!/bin/bash

export DATADIR=$1
declare -a SUBC_IDS=($2)
export SUBC_LAYER=$3
export VAR=$4
export CALC_ALL=$5

export TMPTAX=$DATADIR/tmp
export OUTDIR=$DATADIR/tmp/r_univar


##################################
######     START GRASS
grass78 -f --gtext --tmp-location $SUBC_LAYER  <<'EOF'


if echo "$VAR" | grep -v -q "order_vect" ; then

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

fi

# In case of the order_vect files, which have a different structure
if echo "$VAR" | grep  -q "order_vect" ; then

  VARNAME=$(basename $VAR .gpkg)

  if [ $CALC_ALL == 0 ]; then

    # Variable with the subcatchment ids of interest, has to be comma separated
    export SUBC_IDS=$(awk -F" =" '{print $1 }' $TMPTAX/reclass_rules.txt | head -n-1 | sed -z 's/\n/,/g;s/,$/\n/')


  # Extract stats for all the subcatchments,
  elif [ $CALC_ALL == 1 ]; then

    # Import subc layer to get the ids of all the subcatchments
    r.in.gdal --o input=$SUBC_LAYER output=subc

    # Get the ids of all the subcatchments (-n: don't report null value, -1: vertical output)
    # and format it to the desired format for sql query
    export SUBC_IDS=$(r.describe -1 -n subc | sed -z 's/\n/,/g;s/,$/\n/')

    # #This is in case we manage to filter the gpkg in another way:
    # v.in.ogr -r --o input=$DATADIR/$VAR layer=SELECT \
    # output=stre_cl type=line

  fi

    # The array ($SUBC_IDS) must be comma separated for v.in.ogr to work
    v.in.ogr -r --o input=$DATADIR/$VAR layer=SELECT \
        output=stre_cl type=line key=stream where="stream IN ($SUBC_IDS)"


  v.db.select stre_cl column=stream,strahler,horton,shreve,hack,topo_dim,scheidegger,drwal_old,length,stright,sinosoid,cum_length,out_dist,source_elev,outlet_elev,elev_drop,out_drop,gradient    \
  file=$OUTDIR/stats_${VARNAME}.csv separator=comma  --overwrite

  # Replace the column name 'stream' by 'subc_id'
  sed -i 's/stream/subc_id/g'  $OUTDIR/stats_${VARNAME}.csv

fi

EOF

