#!/bin/bash
stack --local-bin-path ./bin --copy-bins build
cp ./bin/tir ./python/tir
cd python 
pip install .
