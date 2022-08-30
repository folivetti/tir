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
          rev = "986fe161ef78d02683cc400107c9356cb1efb8aa";
          sha256 = "";
        };
        myevo = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo = "evolution";
          rev = "1825d0a761704b32179614dd5cf7d9d501ed1760";
          sha256 = "";
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
        #tir = pkgs.haskell.packages.ghc.callCabal2nixWithOptions "tir" (./.) "" {}; # "--install-method=copy --installdir=./python" { };

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
