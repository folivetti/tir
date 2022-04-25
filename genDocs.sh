#!/bin/bash
stack exec -- haddock --html src/MachineLearning/*.hs src/MachineLearning/Model/*.hs src/MachineLearning/TIR/*.hs src/MachineLearning/Utils/*.hs --hyperlinked-source --odir=docs --quickjump
