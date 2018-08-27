#!/bin/bash

ATTACH_LIST="mailx -s '[ALFRESCO Results]: `pwd`'"
for i in $( ls *.png ); do
	ATTACH_LIST+=" -a ${i}"
done;
for i in $( ls *_df_*.RData ); do
        ATTACH_LIST+=" -a ${i}"
done;

#if [ $4 -eq 1 ]
#then
#for i in $( ls FRP/*FRP.RData ); do
#        ATTACH_LIST+=" -a ${i}"
#done;
#fi

ATTACH_LIST+=" -a $1 $2 < $3"

eval $ATTACH_LIST
