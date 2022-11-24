#!/bin/sh

export BAS=$1
export OUT=$2
## $1 is the raster tile or spatial vector path
## $2 is the output path

if [[ $(find "$BAS" -name "*tif") ]];
	then
	# merge raster file by first creating a vrt and then do the merge
	 gdalbuildvrt $BAS/basin.vrt $BAS/*.tif
	 rm -f $BAS/basin.tif
	 gdal_translate -co COMPRESS=DEFLATE -co ZLEVEL=9 $BAS/basin.vrt $OUT/basin.tif
	 rm -f $BAS/basin.vrt
	else
	 # merge vector file
	 ogrmerge.py -single -progress -skipfailures -overwrite_ds -f GPKG -o $BAS/basin.gpkg  $BAS/*.gpkg 
	 rm -f $BAS/basin_dissolved.gpkg  
	 ogr2ogr  -nlt POLYGON -dialect sqlite -sql "SELECT ST_Union(ST_MakeValid(geom)),"ID" FROM merged GROUP BY "ID" " $OUT/basin_dissolved.gpkg $BAS/basin.gpkg
	 rm -f $BAS/basin.gpkg 
	fi