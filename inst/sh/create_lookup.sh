#! /bin/bash


## script to create :
## 1. look up table of tiles ids (e.g. h00v00) and new integer id
## 2. look up table of tiles ids (e.g. h00v00) and computational units ids
## 3. vrt file of all tiles with new integer id

##   script runs in IGB server 3

export OUT=/mnt/shared/forRpackage/
export DAT=/mnt/shared/hydrography90m_v1_2022_all_data_OLD/lbasin_tiles_final20d_40p/
export COMPUNIT=/mnt/shared/hydrography90m_v1_2022_all_data_OLD/lbasin_compUnit_overview/lbasin_compUnit.tif

mkdir $OUT/tiles
mkdir $OUT/tmp

### create look up table of new code and tiles ids (there are 116 tiles)
paste -d" " <(printf "%s\n" {1..116}) \
    <(find $DAT -name "*.tif" | awk -F[/_] '{print $15}') > $OUT/lookuptable_newID_tile.txt

CreateLookupTable(){

    cod=$1   #  going to each row 1..116

    # extract the new value for the raster
    nva=$(awk -v cod="$cod" 'FNR == cod {print $1}' $OUT/lookuptable_newID_tile.txt)
    # extract the tile id
    tid=$(awk -v cod="$cod" 'FNR == cod {print $2}' $OUT/lookuptable_newID_tile.txt)

    r=$(find $DAT -name "lbasin_${tid}_40p.tif")

    # create new file with new value for the tile
    pkgetmask -co COMPRESS=LZW -co ZLEVEL=9  \
        -i $r -o $OUT/tiles/tile_${tid}.tif -min 0 -nodata 0 -data $nva

    # crop the computational unit file to the extent of the tile
    gdal_translate \
        -projwin $( pkinfo -i $OUT/tiles/tile_${tid}.tif -bb | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' ) \
        -co COMPRESS=LZW -co ZLEVEL=9 $COMPUNIT  $OUT/tmp/compunit_${tid}.tif
    
    # extract compunit ids 
    cu=($(pkstat -hist -i $OUT/tmp/compunit_${tid}.tif | awk '$1 > 0 && $2 > 0 {print $1}'))

    # add tile id and compunit id to look up table
    if [[ "${!cu[@]}" == 1 ]] 
        then
            echo "$tid $cu" >> $OUT/lookuptable_tile_cu.txt
        else
            for i in ${cu[@]}
            do
                echo "$tid $i" >> $OUT/lookuptable_tile_cu.txt
            done
    fi

    rm $OUT/tmp/compunit_${tid}.tif
}

export -f CreateLookupTable
time parallel -j 30 --delay 5 CreateLookupTable ::: {1..116}

# build tiles vrt
gdalbuildvrt $OUT/tiles_global.vrt $(find $OUT/tiles -name "tile*.tif")
