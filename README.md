# TIR: Transformation-Interaction-Rational Symbolic Regression (v1.0)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/folivetti/tir/blob/main/LICENSE)

**TIR** is a fast and simple Evolutionary Algorithm for Symbolic Regression developed in Haskell. Check out the API [documentation](https://folivetti.github.io/tir/) if you want to extend the code.

## Dependencies

For Haskell-only:

- BlAS
- LAPACK
- GSL

For Python wrapper:

- Numpy
- Pandas
- Scikit-Learn

## Installation

1. Clone the repository with `git clone https://github.com/folivetti/tir.git`.

### Using Haskell stack

2. Install the Haskell Stack tool following the instructions at [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/).

3. Run `install_stack.sh`

### Using ghcup

2. Run `install_ghcup.sh` (this will also install ghcup)

### Using nix flake

2. Run `install_nix.sh`

## Running

In order to run the algorithm, first create the training and test set files as a comma separated values without a header (see `datasets` folder for some examples) and then create a config file for the experiment (see `configs` folder for some examples).

The config file is split into three sections where you can set different hyperparameters:

```
[IO]
train = path and name of the training set
test  = path and name of the test set
task  = Regression
log   = PartialLog "path and name of the output file"

[Mutation]
krange      = (-3, 3)
transfunctions = [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]
ytransfunctions  = [Id, Exp, Sin]

[Algorithm]
npop      = 1000
ngens     = 500
algorithm = GA
measures  = ["RMSE", "NMSE", "MAE", "R^2"]
task      = Regression
probmut   = 0.8
probcx    = 0.8
seed      = Nothing

[Constraints]
penalty = NoPenalty
shapes  = []
domains = Nothing
varnames = ["x0", "x1"]
```

The `task` parameter can be set to `Regression` or `Classification`, `transfunctions` accepts a list of transformation functions supported (see `src/IT/Eval.hs` block "Transformation Functions"), `measures` accepts a list of error (minimization) functions to use in the report generator (see `src/IT/Metrics.hs` blocks "Regression measures" and "Classification measures"). 
The `penalty` option can be `NoPenalty`, `Len <double value>` or `Shape <double value>`. The `shapes` option is a list of shape constraints, see `src/IT/Shape.hs` for a list of choices. `domains` is either `Nothing` or `Just [min_x0 ... max_x0, ...]` a list of interval of each variable domain. The `algorithm` option can be `GA` or `FI`.

Run the algorithm with the command:

```
stack run config <conf-file> 
```

where <conf-file> is the path and name of the config file.

As an alternative you can use the python wrapper as illustrated in `example.py`.

## Interaction-Transformation

Transformation-Interaction-Rational (TIR) is a representation proposed in [1]() extending IT representation.

## Cite

de França, F. O. (2022). Interaction-Transformation Evolutionary Algorithm for Symbolic Regression. *GECCO 2022*.

Bibtex:

    @article{
    }

    
## Experiments Results

Notice that the results in this repository are not the same as those in the referenced paper due to constant improvements to the source code (that sometimes fails). The original results for tir and every other algorithm in the paper are available at https://github.com/folivetti/tir/releases/tag/0.1
    
## Contact

Maintained by Fabrício Olivetti de França (folivetti at ufabc.edu.br)

## Acknowledgments

This project is supported by Fundação de Amparo à Pesquisa do Estado de São Paulo (FAPESP), grant number 2018/14173-8.

## License

GNU GPLv3, see [LICENSE](LICENSE)
