nix build --impure
cp ./result/bin/tir-exe ./python/tir
cd python 
pip install .
