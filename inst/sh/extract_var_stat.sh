#!/bin/bash

export DATADIR=$(pwd)    #----> make this to be the working directory
cd $DATADIR
declare -a SUBC_IDS=($1)
export SUBC_LAYER=$2
export VAR=$3
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
grass78 -f  --tmp-location  -c $SUBC_LAYER  <<'EOF'


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
#
#
#
#
#
# #### Join environmental layers
#
# [ ! -d $DATADIR/statsOUT ] && mkdir $DATADIR/statsOUT
# OUTSTAT=$DATADIR/statsOUT
#
# ###   Part I
# ###  Join tables from sc08 (note that the rows or records in these tables represent each microbasin)
#
# for VAR in slopgrad elev flow  ; do  #
#     echo "---- Joining ${VAR} ----"
#     if [ -f $OUTSTAT/stats_${VAR}.csv ];then rm $OUTSTAT/stats_${VAR}.csv; fi
#     echo "zone,${VAR}_min,${VAR}_max,${VAR}_range,${VAR}_mean,${VAR}_mean_of_abs,${VAR}_stddev,${VAR}_variance,${VAR}_coeff_var,${VAR}_sum,${VAR}_sum_abs" > $OUTSTAT/stats_${VAR}.csv
#     FILES=$( find $TMPTAX/r_univar/* -name "*${VAR}.csv" ! -name '*atpoint*')
#     cat $FILES > $TMPTAX/tmp_${VAR}.csv
#     awk -F, '!(/zone/)' $TMPTAX/tmp_${VAR}.csv > $TMPTAX/tmp_${VAR}2.csv
#     cat $TMPTAX/tmp_${VAR}2.csv >> $OUTSTAT/stats_${VAR}.csv
#     rm $TMPTAX/tmp_${VAR}.csv $TMPTAX/tmp_${VAR}2.csv
# done
#
# # echo "stream,strahler,out_dist,elev_drop" > $OUTSTAT/stats_order.csv
# # FILES=$( find $TMPTAX/r_univar/* -name '*order*' ! -name '*atpoint*')
# # cat $FILES > $TMPTAX/TMPGEN_order.csv
# # awk -F, '!(/stream/)' $TMPTAX/TMPGEN_order.csv > $TMPTAX/TMPGEN_order2.csv
# # cat $TMPTAX/TMPGEN_order2.csv >> $OUTSTAT/stats_order.csv
# # rm $TMPTAX/TMPGEN_order2.csv $TMPTAX/TMPGEN_order.csv
#
#
# ######################################################################
#
# ### Part II
# ### Assign to the original table, that is, to each Site_ID, the environmental values given in the previous joined tables by using the microbasin as the joining ID
#
# cat $SUBC_IDS | awk 'BEGIN{OFS=",";} {print $1, $6}' > $TMPTAX/tojoin_ENV.csv
#
# for VAR in   flow slopgrad elev ; # also :  slopcmax slopcmin slopdiff
# do
#       if [ "$VAR" == "order" ]
#       then
#         echo "stream,strahler,out_dist,elev_drop"  > $TMPTAX/tmp_${VAR}.csv
#       else
#         echo "zone,${VAR}_min,${VAR}_max,${VAR}_range,${VAR}_mean,${VAR}_stddev" > $TMPTAX/tmp_${VAR}.csv
#       fi
#           for i in $( seq 1 $( awk 'FNR > 1' $SUBC_IDS | wc -l  ) )
#           do
#             subcID=$(cat $TMPTAX/tojoin_ENV.csv | awk -F, -v ROW=$i 'NR==1+ROW {print $2}')
#             echo "calculating $VAR subcatchment $i"
#             line=$(cat $OUTSTAT/stats_${VAR}.csv | awk -F, -v subcID=$SUBC_LAYERID '$1 == subcID')
#                 if [ -z "$line" ]
#                 then
#                     echo "$SUBC_LAYERID,Empty" >> $TMPTAX/tmp_${VAR}.csv
#                 else
#                     echo $line >> $TMPTAX/tmp_${VAR}.csv
#                 fi
#           done
#       paste -d "," $TMPTAX/tojoin_ENV.csv $TMPTAX/tmp_${VAR}.csv > $FINAL/stats_${VAR}_complete.csv
# done
#
# #rm $TMPTAX/tojoin_ENV.csv $TMPTAX/tmp_*.csv
# #rm -rf $TMPTAX/r_univar
#
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_elev.csv') | sort -n  > $TMPTAX/elev_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_flow.csv') | sort -n  > $TMPTAX/flow_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_flow.csv') | sort -n  > $TMPTAX/flow_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_slopcmin.csv') | sort -n  > $TMPTAX/slopcmin_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_slopcmax.csv') | sort -n  > $TMPTAX/slopcmax_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_slopdiff.csv') | sort -n  > $TMPTAX/slopdiff_atpoint.txt
# # cat $(find $TMPTAX/r_univar/* -name 'stats_atpoint_slopgrad.csv') | sort -n  > $TMPTAX/slopgrad_atpoint.txt
#
# ### Values at point are not needed because we will work with subcatchments
#
# # echo "Site_ID,Elevation,FlowAccum,flow,SlopcMin,SlopcMax,SlopDiff,SlopGrad" > $FINAL/elev_flow_slope_atpoint.csv
#
# # paste -d','   \
# #     <(cut -d'|' -f1 $TMPTAX/elev_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/elev_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/flow_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/flow_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/slopcmin_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/slopcmax_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/slopdiff_atpoint.txt) \
# #     <(cut -d'|' -f2 $TMPTAX/slopgrad_atpoint.txt) \
# #     >> $FINAL/elev_flow_slope_atpoint.csv
#
# # rm $TMPTAX/{elev,flow*,slop*}_atpoint.txt
#
# exit
#
#
#
#
#
#
#
