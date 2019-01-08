#! /bin/bash

rcp=$1 #e.g. rcp60
model=$2 # model directory
period=$3 # historical or not (i.e., projected), if years below indicate CRU data
fmo=$4 # e.g., fmo20s10i

if [ "$period" == "historical" ]
then
 modelOut=CRU32 # renaming destination model directory
 yr1=1950
 yr2=2013
else
 modelOut=$model # leaving destination directory same as source
 yr1=2014
 yr2=2099
fi

inDir=/big_scratch/shiny/Runs_Statewide/paul.duffy@neptuneinc.org/$fmo*$rcp*$model/Maps # source Maps directory
outDir=/atlas_scratch/mfleonawicz/alfresco/JFSP/outputs/$fmo/$rcp.$modelOut/Maps # destination Maps directory
mkdir -p $outDir

for year in `seq $yr1 $yr2`;
do
 out=$outDir/$year
 in=$inDir
 mkdir $out
 cp $in/Age*$year.tif $out/
 cp $in/FireScar*$year.tif $out/
 cp $in/Veg*$year.tif $out/
done
