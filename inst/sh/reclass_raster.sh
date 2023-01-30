#!/bin/sh

export RASTER=$1
export RULES=$2
export OUTPUT=$3
export NODATA=$4
export TYPE=$5
export COMP=$6

# Start GRASS GIS session
grass -f --gtext --tmp-location  $RASTER   <<'EOF'

    # Load raster input file
    r.in.gdal --o input=$RASTER  output=raster    --overwrite

    # Reclassify the raster according to the rules
    r.reclass input=raster output=recl_raster rules=$RULES --overwrite

    # Export reclassified raster map
    r.out.gdal input=recl_raster output=$OUTPUT type=$TYPE  format=GTiff nodata=$NODATA  --o -f -m -c createopt="COMPRESS=$COMP"

EOF

exit
