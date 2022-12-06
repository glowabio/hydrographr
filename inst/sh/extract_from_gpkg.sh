#!/bin/bash

export DATADIR=$1
declare -a SUBC_IDS=($2)
export SUBC_LAYER=$3
export VAR=$4
export CALC_ALL=$5

export OUTDIR=$DATADIR/tmp

# In case of the order_vect files, which have a different structure


##################################
######     START GRASS
grass78 -f --gtext --tmp-location $SUBC_LAYER  <<'EOF'


VARNAME=$(basename $VAR .gpkg)

if [ $CALC_ALL == 0 ]; then

# Variable with the subcatchment ids of interest, has to be comma separated
export SUBC_IDS=$(awk -F" =" '{print $1 }' $OUTDIR/reclass_rules.txt | head -n-1 | sed -z 's/\n/,/g;s/,$/\n/')


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

EOF

