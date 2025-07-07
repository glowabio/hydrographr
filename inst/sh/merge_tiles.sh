#!/bin/bash

# directory where input files are located
export dir="$1"

# list of file names (tif or gpkg) separated by commas
export files=($(echo $2 | tr "/" ","))

# directory for output file
export out="$3"

# output file name (including the tif or the gpkg extension)
export outname="$4"

# Column name used for ST_Union
export colname=$5

# Defines the compression type
export compression=$6

# Defines the corresponding compression level
export level=$7

# BIGTIFF=YES/NO required if output tiffs >4GiB
export bigtiff=$8

# remove the output file if it exists
[ -f "$out"/"${outname}"  ] && rm "$out"/"${outname}"

# create an array of all files needed (with complete path)
g=( $(echo $files | sed 's/,/ /g') )
farray=( $(for i in ${g[@]}; do find $dir -name "$i"; done) )


# merge
f=${farray[0]}
if [ ${f: -4} == ".tif" ]
then
    export outname_base="$(basename "$outname" .tif)"
    gdalbuildvrt -overwrite "$out"/merge_${outname_base}.vrt ${farray[@]}
    gdal_translate  -co COMPRESS=$compression -co ZLEVEL=$level \
    -co BIGTIFF=$bigtiff  "$out"/merge_${outname_base}.vrt "$out"/"${outname}"
    rm "$out"/merge_${outname_base}.vrt
elif [ ${f: -4} == "gpkg" ]
then
    export outname_base="$(basename $outname .gpkg)"
    ogrmerge.py -single -progress -skipfailures -overwrite_ds -f VRT \
        -o "$out"/merge_${outname_base}.vrt ${farray[@]}
    ogr2ogr  -nlt PROMOTE_TO_MULTI -makeValid  \
          "$out"/merge_${outname_base}.gpkg "$out"/merge_${outname_base}.vrt
        # Get the Geometry Column name in the merged fil
    export GEOM=$(ogrinfo -al -so "$out"/merge_${outname_base}.gpkg | \
        grep 'Geometry Column' | awk -F' ' '{print $4}')
    export FID=$(ogrinfo -al -so "$out"/merge_${outname_base}.gpkg | \
        grep 'FID Column' | awk -F' ' '{print $4}')
   export ATTR=$(ogrinfo -al -so "$out"/merge_${outname_base}.gpkg | \
    awk 'FNR >=39 {print $1}' | tr ":" "," | sed 's/,$//g' )
    # including all attributes of input files in the merged file:
    # Build SQL fragment with GROUP_CONCAT() for all attributes except $colname
    ATTR_SQL=""
    for a in $ATTR; do
      if [[ "$a" != "$colname" ]]; then
        ATTR_SQL="$ATTR_SQL, GROUP_CONCAT(DISTINCT $a) AS $a"
      fi
    done
    ogr2ogr -f GPKG -nlt PROMOTE_TO_MULTI -dialect sqlite \
      -sql "SELECT $colname $ATTR_SQL, ST_Union(ST_MakeValid($GEOM)) \
      AS geom FROM merged GROUP BY $colname" \
      "$out"/${outname} "$out"/merge_${outname_base}.gpkg

    # including only column name and id:
  #  ogr2ogr -f GPKG -nlt PROMOTE_TO_MULTI -dialect sqlite \
  # -sql "SELECT $colname, ST_Union(ST_MakeValid($GEOM)) AS geom FROM merged GROUP BY $colname" \
  # $out/${outname} $out/merge_${outname_base}.gpkg

    rm "$out"/merge_${outname_base}.vrt "$out"/merge_${outname_base}.gpkg
fi

exit
