#!/bin/bash
export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH:$PWD/bin
bin/tir-penalty cli $1 $2 $3 $4 $5 $6 $7 $8 $9 "${10}" "${11}" "${12}" +RTS -N1
