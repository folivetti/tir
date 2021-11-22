#!/bin/bash
export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH:$PWD/bin
#export LD_PRELOAD=/opt/intel/mkl/lib/intel64/libmkl_core.so:/opt/intel/mkl/lib/intel64/libmkl_rt.so:/opt/intel/mkl/lib/intel64/libiomp5.so:/opt/intel/mkl/lib/intel64/libmkl_sequential.so:/opt/intel/mkl/lib/intel64/libmkl_avx2.so
bin/tir-exe cli $1 $2 $3 $4 $5 $6 $7 $8 $9 "${10}" "${11}" "${12}" +RTS -N1
