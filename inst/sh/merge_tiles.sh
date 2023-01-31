#!/bin/bash

# directory where input files are located
export dir=$1

# list of file names (tif or gpkg) separated by commas
export files=($(echo $2 | tr "/" ","))

# directory for output file
export out=$3

# output file name (including the tif or the gpkg extension)
export outname=$4

# remove the output file if it exists
[ -f $out/${outname}  ] && rm $out/${outname}

# create an array of all files needed (with complete path)
g=( $(echo $files | sed 's/,/ /g') )
farray=( $(for i in ${g[@]}; do find $dir -name "$i"; done) )


# merge
f=${farray[0]}
if [ ${f: -4} == ".tif" ]
then
    export outname_base=$(basename $outname .tif)
    gdalbuildvrt -overwrite $out/merge_${outname_base}.vrt ${farray[@]}
    gdal_translate  -co COMPRESS=DEFLATE -co ZLEVEL=9 \
    $out/merge_${outname_base}.vrt $out/${outname}
    rm $out/merge_${outname_base}.vrt
elif [ ${f: -4} == "gpkg" ]
then
    export outname_base=$(basename $outname .gpkg)
    ogrmerge.py -single -progress -skipfailures -overwrite_ds -f GPKG \
        -o $out/merge_${outname_base}.gpkg ${farray[@]}
    ogr2ogr  -nlt POLYGON -dialect sqlite \
        -sql "SELECT ST_Union(ST_MakeValid(geom)),"ID" FROM merged GROUP BY "ID" " \
        $out/${outname} $out/merge_${outname_base}.gpkg
    rm $out/merge_${outname_base}.gpkg
fi

exit


#export BAS=$1
#export OUT=$2
### $1 is the raster tile or spatial vector path
### $2 is the output path
#
# if (find "$BAS" -name "*tif") | grep -q $BAS; then
#	# merge raster file by first creating a vrt and then do the merge
#	 gdalbuildvrt -overwrite $BAS/basin.vrt $BAS/*.tif
#	 rm -f $BAS/basin.tif
#	 gdal_translate -co COMPRESS=DEFLATE -co ZLEVEL=9 $BAS/basin.vrt $OUT/basin.tif
#	 rm -f $BAS/basin.vrt
#
# elif (find "$BAS" -name "*gpkg") | grep -q $BAS; then
#	 # merge vector file
#	 rm -f $BAS/basin.gpkg
#	 ogrmerge.py -single -progress -skipfailures -overwrite_ds -f GPKG -o $BAS/basin.gpkg  $BAS/*.gpkg
#	 rm -f $BAS/basin_dissolved.gpkg
#	 ogr2ogr  -nlt POLYGON -dialect sqlite -sql "SELECT ST_Union(ST_MakeValid(geom)),"ID" FROM merged GROUP BY "ID" " $OUT/basin_dissolved.gpkg $BAS/basin.gpkg
#	 rm -f $BAS/basin.gpkg
# fi
