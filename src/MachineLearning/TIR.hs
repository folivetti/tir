{-|
Module      : MachineLearning.TIR
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

The TIR expression  represents a function of the form:

\[
f(x) = g(\sum_{i}{w_i \cdot t_i(\prod_{j}{x_j^{k_{ij}})}} / (1 + \sum_{i}{w_i \cdot t_i(\prod_{j}{x_j^{k_{ij}})}})
\]

with \(t_i\) being a transformation function, \(g\) an invertible function, \(w_i\) a linear coefficient, \(k_{ij}\) 
the interaction strength.

Any given expression can be represented by two lists of terms, with each term
being composed of a transformatioon function and an interaction.
The transformation function is represented by a `Function` sum type.
The interaction is represented as a list of tuple of ints where the key is the 
predictor index and the value is the strength of the predictor in this
term. Strengths with a value of zero are omitted.
-}
module MachineLearning.TIR where

import Control.Evolution
import Control.Monad.State.Strict
import Data.List.Split
import Data.SRTree
import Control.DeepSeq                           (NFData, rnf)

import Data.SRTree.Print                         (showDefault)
import Data.Vector                               (Vector)
import qualified Data.Vector             as V
import qualified Data.Vector.Storable    as VS
import qualified Numeric.LinearAlgebra   as LA

-- | `TIR` is a record type composed of the external
-- function `_funY` of type `Function`, numerator `_p`
-- and denominator `_q` of type `Sigma`
data TIR = TIR { _funY :: Function
               , _p :: Sigma
               , _q :: Sigma
               } deriving Show

instance NFData TIR where
  rnf _ = ()

-- | `Sigma` is just a list of terms `Pi`
type Sigma = [Pi]
-- | `Pi` is a triple composed of a coefficient, a `Function`
-- and a list of tuples where `(ix, k)` represents `x ! ix ^ k`.
type Pi    = (Double, Function, [(Int, Int)])

-- | We store thee dataset as a vector of columns. 
-- Each vector is stored a `Storable`-based vector.
type Column a   = LA.Vector a

-- | A dataset is a `Vector` of `Column`
type Dataset a  = Vector (Column a)

-- | A constraint is a function that gets a symbolic tree
-- as an input and returns non negative `Double` representing
-- how much a constraint was violated.
type Constraint = SRTree Int Double -> Double

-- | An individual in the population is composed of
-- the chromossome, a vector of fitness, a list of
-- coefficients (for multiclass problems it stores
-- one vector of coefficient per class),
-- the constraint violation, the size of the expression,
-- and the penalty value.
data Individual = Individual { _chromo   :: TIR
                             , _fit      :: [Double]
                             , _accs     :: [Double]
                             , _weights  :: [LA.Vector Double]
                             , _constr   :: Double
                             , _len      :: Int
                             , _penalty  :: Double
                             }

-- | creates an unevaluated individual.
createIndividual :: TIR -> Individual
createIndividual tir = Individual tir [] [] [] 0.0 0 0.0

-- | calculates the penalized fitness.
penalizedFit :: Individual -> Double
penalizedFit t = (head . _fit) t + _penalty t
{-# INLINE penalizedFit #-}

-- | replaces the coefficients of a TIR expression
replaceConsts :: TIR -> V.Vector Double -> TIR
replaceConsts (TIR g p q) ws = TIR g p' q'
  where
    (p', ws1) = runState (traverse replaceWeight p) (V.toList ws)
    (q', ws2) = runState (traverse replaceWeight q) ws1

replaceWeight :: Pi -> State [Double] Pi
replaceWeight (w, g, h) = state $ \ws -> case ws of
                                           (wi:ws') -> ((wi, g, h), ws')
                                           []       -> error $ show h -- ((w, g, h), [])

{-
instance Eq Individual where
    t1 == t2 = penalizedFit t1 == penalizedFit t2 
instance Ord Individual where
    t1 <= t2 = penalizedFit t1 <= penalizedFit t2
-}
epsDom x y | abs (x - y) <= eps = EQ
           | otherwise          = compare x y
  where eps = 0.00
{-# inline epsDom #-}

instance Eq Individual where
  t1 == t2 = case epsDom f1h f2h of
               EQ -> all (uncurry (==)) $ zip f1t f2t
               _  -> False
    where
     f1h = penalizedFit t1
     f1t = tail (_fit t1)
     f2h = penalizedFit t2
     f2t = tail (_fit t2)
instance Ord Individual where
  t1 <= t2 = case epsDom f1h f2h of
               EQ -> (any (uncurry (<)) (zip f1t f2t) && all (uncurry (<=)) (zip f1t f2t)) -- lesser
                     || f1t == f2t
               LT -> all (uncurry (<=)) (zip f1t f2t)
               GT -> False
    where
     f1h = penalizedFit t1
     f1t = tail (_fit t1)
     f2h = penalizedFit t2
     f2t = tail (_fit t2)

instance NFData Individual where
  rnf _ = ()

instance Solution Individual where
  _getFitness = _fit
  _isFeasible = (<1e-12) . _constr

-- | creates a symbolic tree from a TIR expression.
assembleTree :: Double -> TIR -> SRTree Int Double
assembleTree bias (TIR f p q) = Fun f ((Const bias + assemble p) / (1 + assemble q))
  where
    -- assemble :: Sigma ix val -> SRTree ix val
    assemble []      = 0
    assemble [p']    = mk p'
    assemble (p':ps) = mk p' + assemble ps

    -- mk :: Pi ix val -> SRTree ix val
    mk (v, g, ts) = Const v * Fun g (foldr (\(ix, k) acc -> acc * Pow (Var ix) k) 1 ts)

-- | pretty print a solution.
prettyPrintsolution :: Individual -> String
prettyPrintsolution sol | Prelude.null (_fit sol) = error "unevaluated solution"
prettyPrintsolution sol = concat [ "Expression:\n", (showDefault . assembleTree bias . _chromo) sol, "\n"
                                 , "Fitness: ", (show . head . _fit) sol, "\n"
                                 , "Constraints: ", (show . _constr) sol, "\n"
                                 , "Length: ", (show . _len) sol, "\n"
                                 , "Penalty: ", (show . _penalty) sol, "\n"
                                 ]

  where bias = V.head $ VS.convert $ head $ _weights sol
