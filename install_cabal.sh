!/bin/bash

#conda activate srbench
cabal install
cp ~/.cabal/bin/tir-exe ./python/tir
cd python 
pip install .
