#!/bin/sh

export DATA=$1
export SUBC=$2

## $1 is the dataset path and $2 the subcatchment path
awk 'FNR > 1 {print $2, $3}' $DATA   | gdallocationinfo -valonly -geoloc  $SUBC


