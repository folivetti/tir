NIXPKGS_ALLOW_BROKEN=1 nix build --impure
cp ./result/bin/tir-exe ./python/tir
cd python 
pip install .
