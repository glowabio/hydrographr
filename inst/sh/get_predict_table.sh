#! /bin/bash


#### parameters

###  1. list of variables of interest: list separated by commas
###  2. list of statistics of interest :  default "ALL" or list separated by commas
###  3. list of neccesary tiles IDs: list separated by commas
###  4. path to environmental tables
###  5. path to output file
###  6. path to temporal folder
###  7. number of cores (if possible) to run internal process in parallel:
###     Highest number needed = (n.tiles * n.variables)


#time  bash dev_create_roi_table.sh \
#  bio1,c10_2020,spi,stright \
#  mean,sd \
#  h18v02,h18v04,h20v02,h20v04 \
#  /data/marquez/vignette/env_tables  \
#  /data/marquez/vignette/out/subc_IDs.txt  \
#  /data/marquez/vignette/out/projectionTB.csv  \
#  /data/marquez/vignette/out  \
#  1



#####  PARAMETERS

# variables of interest
#export var=( bio1 c10_2020 spi stright )
VAR=( $(echo $1 | tr "/" "\n") )
[[ "${#VAR[@]}" -eq 1 ]] && var=($(echo $1)) || var=("${VAR[@]}")
export var

# select summary statistics
# ALL
# c(mean, sd)
ss=( $(echo $2 | tr "/" "\n") )
[[ "${#ss[@]}" -eq 1 ]] && SS=($(echo $2)) || SS=("${ss[@]}")
export SS

# tiles of interest
#export tiles=( h18v02 h18v04 h20v02 h20v04 )
TT=($(echo "$3" | tr "/" "\n"))
[[ "${#TT[@]}" -eq 1 ]] && tiles=($(echo $3)) || tiles=("${TT[@]}")
export tiles

#  path to environmental tables for each tile
export ENVTB=$4

# file with the list of subcatchments IDs
export SUBCIDS=$5
#export SUBCIDS=/data/marquez/vignette/out/subc_IDs.txt

# output file
export OUTFILE=$6
#export OUTFILE=/data/marquez/vignette/out/projectionTB2.csv

# folder to store temporal files: this folder is defined within the R function
export TMP=$7
#export TMP=/data/marquez/vignette/out

# number of cores to run the extraction of information (rows) from tile tables
# (n.tiles * n.variables)
export NCORES=$8
#export NCORES=16


##### ANALYSIS

##################
# Move through each tile and extract the subcatchment of interests for
# all variables

subsetTB(){
    TL=$1  # tile
    k=$2   # variable
    TB=$(find $ENVTB -name "${k}_${TL}.txt")
    awk 'NR==FNR {a[$1]; next} FNR==1 || $1 in a' \
     $SUBCIDS $TB \
     | awk 'NR > 1 {for(i=1; i<=NF; i++) $i+=0}1' CONVFMT="%.3f" \
     >  $TMP/ENV_${TL}_${k}.txt
}

export -f subsetTB
parallel -j $NCORES subsetTB ::: ${tiles[@]} ::: ${var[@]}


##################
#   Subset the tables if user is only interested in few statistics (e.g., mean)

if [[ "${SS[@]}" != 'ALL' ]]    # run only if user do not select ALL
then
    for TB in $(find $TMP -name "ENV_*.txt")
    do
        NR=$(awk 'NR == 1 {print NF}' $TB)
        [[ "$NR" != 6 ]] && continue

        RAND_STRING=$(xxd -l 8 -c 32 -p < /dev/random)

        read -a header <  $TB   # read first line into array "header"
        declare -a arr=() # array to store the position in which the subCid name is

        for s in ${SS[@]}
        do
            for i in ${!header[@]}               # iterate through array indexes
            do
                if [ "${header[i]}" = "$s" ]    # find column equal the pattern
                then
                    arr+=( "$[++i]"  )
                fi
            done
        done

        printf -v joined '%s,' "${arr[@]}"

        cut -d" " -f $(echo "1,${joined%,}") $TB \
        > $TMP/ENV_${RAND_STRING}.txt

        mv $TMP/ENV_${RAND_STRING}.txt $TB
    done
fi


##################
### join all tables for the same variable available in the different tiles
echo ${var[@]} | xargs -n 1 -P $NCORES bash -c $'

X=$1

listf=( $(find $TMP -name "ENV_*_${X}.txt")  )

cat ${listf[@]} > $TMP/aggreg_${X}.txt
awk \'FNR == 1; FNR > 1 && /^[0-9]/\' $TMP/aggreg_${X}.txt \
    > $TMP/aggreg_${X}_tmp1.txt
sort -g $TMP/aggreg_${X}_tmp1.txt > $TMP/aggreg_${X}.txt

read -a header < $TMP/aggreg_${X}.txt

declare -a elem=()
for e in mean min max sd range
do
    [[ ${header[@]} =~ $e ]] && elem+=1 || elem+=0
done

# set the right name for the header (e.g. bio1_mean)
if [[ "${elem[@]}" -gt 0 ]]; then

    nof=( ${header[@]:1} )
    allh=( ${header[0]} $(echo "${nof[@]/#/${X}_}") )
    echo ${allh[@]} > $TMP/aggreg_${X}_tmp2.txt
    awk \'NR>1\' $TMP/aggreg_${X}.txt  >> $TMP/aggreg_${X}_tmp2.txt
    mv $TMP/aggreg_${X}_tmp2.txt $TMP/aggreg_${X}.txt
fi

rm $TMP/aggreg_${X}_tmp*.txt

' _


#################
## join all the environmental variables together
paste -d" " $(find $TMP/aggreg_*.txt) > $TMP/all_var_full.txt

## the previous line creates repetition of the subcID column
## Chunk to delete the subCid column
read -a header < $TMP/all_var_full.txt  # read first line into array "header"
declare -a arr=() # array to store the position in which the subCid columns are

for i in ${!header[@]}               # iterate through array indexes
do
    if [ "${header[i]}" = "subcID" ]    # find column equal the pattern
    then
        arr+=( "$[++i]"  )
    fi
done

if [[ "${#arr[@]}" -gt 1 ]]
then
    printf -v joined '%s,' "${arr[@]:1}"  # remove from array the first column

##  remove subcID columns (except column 1) and make the table comma separated
    cut -d" " --complement -f $(echo "${joined%,}") $TMP/all_var_full.txt \
    | tr -s ' ' ',' > $TMP/all_var_trim.csv

##  remove duplicates and create final output table
    [[ "${#tiles[@]}" -gt 1  ]] && awk -F, '!a[$0]++'  $TMP/all_var_trim.csv > $OUTFILE || cp $TMP/all_var_trim.csv $OUTFILE

else

    cat $TMP/all_var_full.txt |  tr -s ' ' ',' > $TMP/all_var_trim.csv
##  remove duplicates and create final output table
    [[ "${#tiles[@]}" -gt 1  ]] && awk -F, '!a[$0]++'  $TMP/all_var_trim.csv > $OUTFILE || cp $TMP/all_var_trim.csv $OUTFILE

fi


#########################
# remove temporal files
rm $TMP/aggreg*
rm $TMP/ENV*
rm $TMP/all_var_trim.csv
rm $TMP/all_var_full.txt



exit

