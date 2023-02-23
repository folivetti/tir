#!/bin/bash

sed "s/log\/$1/log\/$1_$2$3/; s/= FS/= $2/; s/$1.csv/$1$3.csv/" diversity/$1.cfg > tmp.cfg
stack run config tmp.cfg +RTS -N2 
