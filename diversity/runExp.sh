#!/bin/bash

sed "s/log\/$1/log\/$1_$2$3/; s/= FS/= $2/; s/$1.csv/$1$3.csv/" diversity/$1.cfg > tmp.cfg
for i in {1..30}
    do
        stack run config tmp.cfg | grep "Avg. dist.:" | awk -F: '{print $2}' >> diversity/avgdist/$1_$2$3_avgdist.csv
    done
