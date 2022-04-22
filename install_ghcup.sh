!/bin/bash

# install ghcup
if [! -d "~/.ghcup" ]; then
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash
  export PATH=$PATH:~/.ghcup/bin:~/.cabal/bin 
fi

#conda activate srbench
cabal install
cp ~/.cabal/bin/tir-exe ./python/tir
cd python 
pip install .
