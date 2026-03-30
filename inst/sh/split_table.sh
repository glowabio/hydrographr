#!/bin/bash

# source:
# modify from answer 14: user marco
# https://stackoverflow.com/questions/1411713/how-to-split-a-file-and-keep-the-first-line-in-each-of-the-pieces

# parameter 1: table to split
#export TB=/mnt/shared/danube/out/danube_predictTB.csv
 export TB=$1

# parameter 2: path to directory where the (subset)tables will be stored
# export DIR=/mnt/shared/danube/out
export DIR=$2

# parameter 3: number of rows selected to split the table
#export NUM=500000
export NUM=$3

awk -v nrows="${NUM}"  -v dir="${DIR}"  'NR==1{
        header=$0; 
        count=1; 
        print header > dir "/predTB_" count ".csv"; 
        next 
     } 

     !( (NR-1) % nrows){
        count++; 
        print header > dir "/predTB_" count ".csv";
     } 
     {
        print $0 > dir "/predTB_" count ".csv"
     }' $TB


