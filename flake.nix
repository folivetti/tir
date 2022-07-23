{
  description = "tir";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";
  inputs.pypi-deps-db.url = "github:DavHau/pypi-deps-db";

  inputs.mach-nix = {
    url = "github:DavHau/mach-nix";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
      pypi-deps-db.follows = "pypi-deps-db";
    };
  };

  outputs = { self, nixpkgs, flake-utils, mach-nix, pypi-deps-db }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        mysrtree = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo = "srtree";
          rev = "2b376f6d7a9cacf65978e9f8718c3c0d330b4cba";
          sha256 = "PLwPJxJmrCohYchWuXKtq5o6+H7Nl8WtZf6qEZpNhYw=";
        };
        myshapeconstraint = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo = "shape-constraint";
          rev = "88015baff58ef88deaf47980844becfe9ef9a110";
          sha256 = "jHOs2XV2E8NWYH3Nam+AIezyYYDyS9v8aZUxpVJfeAM=";
        };
        myevo = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo = "evolution";
          rev = "e38a96bd65beed4a47cffc141ee18b4973b5f5ed";
          sha256 = "9eyym7BLmO+Zcktqw+8NmRN6NHakV/6/MYZbawgFUp4=";
        };
        mymodal = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo = "modal-interval";
          rev = "d43ec184314c2bb61aa8bbb974866d68d14b2e45";
          sha256 = "sGsWZpdmi/Eo8QXCCD9193wSOJzzR+Xc0AMnUL2YV1k=";
        };
        config = {
          packageOverrides = pkgs: {
            haskell = pkgs.haskell // {
              packages = pkgs.haskell.packages // {
                ghc = pkgs.haskell.packages.ghc923.override {
                  overrides = self: super: {
                    # mltool = pkgs.haskell.lib.dontCheck super.mltool;
                    srtree = super.callCabal2nix "srtree" mysrtree { };
                    shape-constraint =
                      super.callCabal2nix "shape-constraint" myshapeconstraint
                      { };
                    evolution = super.callCabal2nix "evolution" myevo { };
                    modal-interval =
                      super.callCabal2nix "modal-interval" mymodal { };
                  };
                };
              };
            };
          };
        };

        pkgs = import nixpkgs { inherit system config; };
        mach = mach-nix.lib.${system};

        tir = pkgs.haskell.packages.ghc.callCabal2nix "tir" ./. { };

        pytir = mach.buildPythonPackage rec {
          python = "python39";
          ignoreDataOutdated = true;

          pname = "pyTIR";
          version = "1.2.0.0";

          requirements = ''
            scikit-learn
            numpy
            pandas
          '';

          src = self;
          preConfigure = ''
            cd python
          '';
        };

      in rec {
        defaultPackage = pytir;
        devShell = pkgs.mkShell {
          name = "pytir dev";
          buildInputs = [ tir pytir ];

          shellHook = ''
            PYTHONPATH=$PYTHONPATH:"${tir}"
          '';
        };
      });
}
