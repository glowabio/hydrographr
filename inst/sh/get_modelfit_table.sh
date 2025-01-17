#! /bin/bash

# time bash dev_create_model_table.sh  \
#   /data/marquez/vignette/out/spp.csv  \
#   /data/marquez/vignette/out/projectionTB.csv  \
#   /mnt/shared/sosw/tmp/danube_subcatchments.tif  \
#   10000  \
#   /data/marquez/vignette/out  \
#   /data/marquez/vignette/out/model_table.csv



### INPUT

## file, comma separated, with table with species name and coordinates
## be sure no subcatchment duplicates!!!
export SPP=$1
#export SPP=/data/marquez/vignette/out/spp.csv

# crear archivo solo con datos de la especie de interes
#awk -F, -v SPP="Huso huso" 'BEGIN{OFS=",";} NR == 1 || $2 == SPP {print $2, $3, $4}' \
#    /mnt/shared/sosw/sppTB/fish_danube.csv > /data/marquez/vignette/out/spp.csv 

## projection table created with function create_roi_table.sh
export PTB=$2
#export PTB=/data/marquez/vignette/out/projectionTB2.csv

## path to subcatchment file
export SUBC=$3
#export SUBC=/mnt/shared/sosw/tmp/danube_subcatchments.tif

### number of pseudoabsences
export ABS=$4
#export ABS=10000

## temporal folder
export TMP=$5
#export TMP=/data/marquez/vignette/out

## output file, the model table
export OUTF=$6
#export OUTF=/data/marquez/vignette/out/model_table.csv


### ANALYSIS

## Presence Data

# species name
export SPPN=$(awk -F, 'NR == 2 {print $1}' $SPP)

# line to calculate the number of rows in the presence table (no counting header)
C=$( awk -F, '{print NR}' $SPP | tail -n2 | head -n1 )

# adicionar al archivo anterioir las columnas de subcatchment id
# add new columns: subcatchment id and presence/absence column (add 1 presences)
paste -d "," \
    <(printf "%s\n" subcID $(awk -F, 'FNR > 1 {print $2, $3}' $SPP | gdallocationinfo -valonly -geoloc $SUBC)) \
    <(printf "%s\n" PresAbs $(printf '1%.0s\n' $(eval "echo {1.."$(($C))"}") )) \
    > $TMP/tmp1.csv

# extract from projection table the environmental data for the presences 
awk -F, 'NR==FNR {a[$1]; next} FNR==1 ||  $1 in a' \
    $TMP/tmp1.csv $PTB  >  $TMP/tmp2.csv

# join tables
paste -d"," \
    <(sort -t, -g -k1 $TMP/tmp1.csv) \
    <(sort -t, -g -k1 $TMP/tmp2.csv) \
    | cut -d"," --complement -f 3 \
    > $TMP/pa_env_tmp.csv

## (Pseudo)absences

if [[ "$ABS" -eq 0  ]]
then 
    cp $TMP/pa_env_tmp.csv $OUTF
else

    ####   Procedure by selecting random rows from predict_table $PTB
    shuf -n$ABS $PTB --output=$TMP/abs_tmp1.csv

    #  number of rows in the absence table 
    A=$( wc -l < $TMP/abs_tmp1.csv )

    # create preliminary table with same format as tmp1
    paste -d"," \
        <(awk -F, '{print $1}' $TMP/abs_tmp1.csv) \
        <(printf '0%.0s\n' $(eval "echo {1.."$(($A))"}")) \
        <(cut -d"," --complement -f 1 $TMP/abs_tmp1.csv) \
        > $TMP/abs_tmp2.csv

    ### Join presences and absences
    cat $TMP/pa_env_tmp.csv $TMP/abs_tmp2.csv > $OUTF  
fi

### remove temporal files
rm $TMP/*tmp* 

exit


