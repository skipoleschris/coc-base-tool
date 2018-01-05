#!/bin/bash

DIR=$1
SIZE=$2
WIDTH=`expr $SIZE \* 15`
HEIGHT=`expr $SIZE \* 15`
DIMENSION="${WIDTH}x${HEIGHT}"


cd data/images/$DIR/original
for f in *.png
do
  convert "$f" -resize $DIMENSION -gravity center -extent $DIMENSION "../tile/$f"
done
cd ../../../..

