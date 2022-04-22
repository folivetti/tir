#!/bin/bash
stack --local-bin-path ./bin --copy-bins build
cp ./bin/tir-exe ./python/tir
cd python 
pip install .
