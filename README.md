# TIR: Transformation-Interaction-Rational Symbolic Regression (v1.0)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/folivetti/tir/blob/main/LICENSE)

**TIR** is a fast and simple Evolutionary Algorithm for Symbolic Regression developed in Haskell. Check out the [documentation](https://folivetti.github.io/tir/) if you want to extend the code.

Transformation-Interaction-Rational (TIR) searches for models of the form:

![eq](https://latex.codecogs.com/gif.image?\dpi{110}f_{TIR}(\mathbf{x,&space;w_p,&space;w_q})&space;=&space;g\left(\frac{p(\mathbf{x,&space;w_p})}{1&space;&plus;&space;q(\mathbf{x,&space;w_q})}\right))

with

\(
p_i(x,k) = \prod_{j}{x_j^k_{ij}}
\)

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

### Using Haskell Stack

2. Install the Haskell Stack tool following the instructions at [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/).

3. Run `install_stack.sh`

### Using Cabal

2. Run `install_cabal.sh`

### Using Nix flake

2. Run `install_nix.sh`

## Running

In order to run the algorithm, first create the training and test set files as a comma separated values without a header (see `datasets` folder for some examples) and then create a config file for the experiment (see `configs` folder for some examples).

The config file is split into three sections where you can set different hyperparameters:

```
[IO]
train = path and name of the training set
test  = path and name of the test set
log   = PartialLog "path and name of the output file"

[Mutation]
krange      = (-3, 3)
transfunctions = [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]
ytransfunctions  = [Id, Exp, Sin]

[Algorithm]
npop      = 1000
ngens     = 500
algorithm = GPTIR
measures  = ["RMSE", "NMSE", "MAE", "R^2"]
task      = Regression
probmut   = 0.8
probcx    = 0.8
seed      = Nothing

[Constraints]
penalty = NoPenalty
shapes  = []
domains = []
evaluator = Nothing
```

Run the algorithm with the command:

```
stack run config <conf-file> 
```

where <conf-file> is the path and name of the config file.

As an alternative you can use the python wrapper as illustrated in `example.py`.

## Configuration options

### IO

- `train` - path of a comma separated file containing the training data
- `test` - path of a comma separated file containing the test data (use the same as `train` if you don't want to use a test set)
- `log` - `Screen` for screen-only results, `PartialLog "directory"` folder of where to store a partial log of the final results, `FullLog "directory"` folder where to store a full log of the whole evolutionary process.


### Mutation

- `krange` - tuple of integers of the minimum and maximum exponents of the interaction terms.
- `transfunctions` - list of transformation functions. Available functions are `Id, Abs, Sin, Cos, Tan, Sinh, Cosh, Tanh, ASin, ACos, ATan, ASinh, ACosh, ATanh, Sqrt, Square, Log, Exp`.
- `ytransfunctions` - list of invertible transformation function. Available functions are `Id, Sin, Cos, Tan, Tanh, ASin, ACos, ATan, ATanh, Sqrt, Square, Log, Exp`.

### Algorithm

- `npop` - population size
- `ngens` - number of generations
- `algorithm` - algorithm: `GPTIR` for genetic programming TIR or `SCTIR` for shape-constrained genetic programming TIR.
- `measures`  - list of performance measures to calculate: `"RMSE", "NMSE", "MAE", "R^2"`. The first measure is the fitness function.
- `task` - Regression / Classification (currently unsupported)
- `probmut` - mutation probability
- `probcx` - crossover probability
- `seed` - random seed to be used: `Nothing` for default seed (i.e., current time), `Just 42` for the random seed 42.

## Constraints

- `penalty` - the penalty term to be added to the fitness function: `NoPenalty`, `Len 0.01` (0.01 times the number of nodes of the expression), `Shape 0.01` (0.01 times the number of shape-constraint violations).
- `shapes`  - list of shape-constraints (refer to https://github.com/folivetti/shape-constraint).
- `domains` - list of tuples with the minimum and maximum values of each variable.
- `evaluator` - constraint evaluator: `Nothing`, `Just InnerInterval`, `Just OuterInterval`, `Just (Sampling 100)` (evaluate with 100 samples), `Just Hybrid`, `Just Bisection` (refer to https://github.com/folivetti/shape-constraint).

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
