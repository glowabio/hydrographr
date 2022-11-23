#!/bin/sh

export BAS=$1
## $1 is the raster tile or spatial vector path

for i in "$BAS"; do
	 gdalbuildvrt basin.vrt $BAS/*.tif 
	 gdal_translate -co COMPRESS=DEFLATE -co ZLEVEL=9 basin.vrt  basin.tif
	 rm -f basin.vrt
	 ogrmerge.py -single -progress -skipfailures -overwrite_ds -f GPKG -o basin.gpkg  $BAS/*.gpkg 
	 rm -f basin_dissolved.gpkg  
	 ogr2ogr  -nlt POLYGON -dialect sqlite -sql "SELECT ST_Union(geom),"ID" FROM merged GROUP BY "ID" " basin_dissolved.gpkg basin.gpkg
	 rm -f basin.gpkg  
done