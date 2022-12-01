#!/bin/bash

export DATADIR=$1    #----> make this to be the working directory
cd $DATADIR
declare -a SUBC_IDS=($2)
export SUBC_LAYER=$3
export VAR=$4
echo $VAR

# export FINAL=$4

export TMPTAX=$DATADIR/tmp
# [ ! -d $TMPTAX ] && mkdir $TMPTAX

# [ ! -d $TMPTAX/r_univar ] && mkdir $TMPTAX/r_univar
export OUTDIR=$TMPTAX/r_univar


# for i in  "${SUBC_IDS[@]}"; do
# echo "$i" =  1 >> $TMPTAX/reclass_rules.txt
# done
# echo '* = NULL' >> $TMPTAX/reclass_rules.txt



##################################
######     START GRASS
grass78 -f --gtext --tmp-location $SUBC_LAYER  <<'EOF'


if echo "$VAR" | grep -v -q "order" ; then

    VARNAME=$(basename $VAR .tif)
    # Read in subcatchment raster
    r.in.gdal --o input=$SUBC_LAYER output=subc

    # Procedure to create a subcatchment raster but only with the given subcatchments
    r.reclass --o input=subc output=subc_recl rules=$TMPTAX/reclass_rules.txt
    r.mapcalc --o "subconly = if(subc_recl==1,subc,null())"
    echo 'ekana rmapclacl'
    # Calculate the statistics of the given variable for each microbasin and save in a temporary .csv file
    r.external  input=$VAR  output=$VARNAME --overwrite
    echo 'ekana r.external'
    echo "subc_id,${VARNAME}_min,${VARNAME}_max,${VARNAME}_range,${VARNAME}_mean,${VARNAME}_sd" \
         > $OUTDIR/stats_${VARNAME}.csv
    r.univar -t --o map=$VARNAME zones=subconly  separator=comma |  awk -F, 'BEGIN{OFS=",";} NR>1 {print $1, $5, $6, $7, $8, $10}'  \
      >> $OUTDIR/stats_${VARNAME}.csv
    echo 'ekana r univar'
fi


if echo "$VAR" | grep  -q "order" ; then

    VARNAME=$(basename $VAR .gpkg)
    # Variable with the subcatchment ids of interest, has to be comma separated
    export SUBC_IDS=$(awk -F" =" '{print $1 }' $TMPTAX/reclass_rules.txt | head -n-1 | sed -z 's/\n/,/g;s/,$/\n/')
    echo $SUBC_IDS
    # The array ($SUBC_IDS) must be comma separated for v.in.ogr to work
    v.in.ogr  --o input=$VAR layer=SELECT \
        output=stre_cl type=line key=stream where="stream IN ($SUBC_IDS)"


    v.db.select stre_cl column=stream,strahler,horton,shreve,hack,topo_dim,scheidegger,drwal_old,length,stright,sinosoid,cum_length,out_dist,source_elev,outlet_elev,elev_drop,out_drop,gradient    \
 file=$OUTDIR/stats_${VARNAME}.csv separator=comma  --overwrite

    sed -i 's/stream/subc_id/g'  $OUTDIR/stats_${VARNAME}.csv

fi

EOF
# rm $TMPTAX/reclass_rules.txt
