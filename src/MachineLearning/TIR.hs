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
import System.Random
import Control.DeepSeq                           (NFData, rnf)
import Data.List                                 (delete)
import Data.SRTree.Print                         (showDefault)
import Data.Vector                               (Vector)
import qualified Data.Vector             as V
import qualified Data.Vector.Storable    as VS
import qualified Numeric.LinearAlgebra   as LA
import MachineLearning.Utils.Config

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

-- | generates a random integer within the specified range.
randomRng :: (Int, Int) -> Rnd Int
randomRng rng = state $ randomR rng
{-# INLINE randomRng #-}

-- | generates a random integer within the specified range excluding zero.
randomRngNZ :: (Int, Int) -> Rnd Int
randomRngNZ rng = do
  x <- randomRng rng
  if x == 0
    then randomRngNZ rng
    else pure x
{-# INLINE randomRngNZ #-}

-- | picks a random element from a list.
randomFrom :: [a] -> Rnd a
randomFrom xs = do
  ix <- randomRng (0, length xs - 1)
  pure (xs !! ix)
{-# INLINE randomFrom #-}

-- | returns a random index of variables provided by the mutation configuration.
randomVar :: MutationCfg -> Rnd (Maybe Int, MutationCfg)
randomVar params = do
  let vars = _vars params
      n    = length vars
  ix <- randomRng (0, n)
  if ix == n
     then pure (Nothing, params)
     else do let x = vars !! ix
             pure (Just x, params{ _vars=delete x vars })

-- | returns a list of random interactions (tuples of variables indeces and exponentes)
-- with parameters provided by the mutation configuration.
randomVars :: MutationCfg -> Rnd [(Int, Int)]
randomVars params = do
  (v, params') <- randomVar params
  k            <- randomRngNZ $ _kRange params
  case v of
    Nothing  -> pure []
    Just var -> do vs <- randomVars params'
                   pure $ (var, k) : vs

-- | returns a random `Pi`
randomPi :: MutationCfg -> Rnd (Maybe Pi)
randomPi params = do
  pis <- randomVars params
  f   <- randomFrom $ _funs params
  if null pis
    then pure Nothing
    else pure $ Just (1.0, f, pis)

-- | returns a random `Sigma`
randomSigma :: MutationCfg -> Int -> Rnd (Sigma, Int)
randomSigma params budget | budget <= 0 = pure ([], budget)
randomSigma params budget = do
  n <- randomRng (0, budget)
  if n == budget
     then pure ([], budget)
     else do term             <- randomPi params
             (terms, budget') <- randomSigma params (budget - spentBudget term)
             case term of
               Nothing -> pure (terms, budget')
               Just t  -> pure (t:terms, budget')

  where
    spentBudget Nothing           = 0
    spentBudget (Just (_, _, ps)) = 1 -- length ps

-- | returns a random `TIR` expression
randomTIR :: MutationCfg -> Rnd TIR
randomTIR params = do
  yf           <- randomFrom $ _yfuns params
  (p, budget') <- randomSigma params $ _budget params
  (q, _)       <- randomSigma params budget'
  if null p
    then randomTIR params
    else pure (TIR yf p q)

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
data Individual = Individual { _chromo  :: TIR
                             , _fit     :: [Double]
                             , _weights :: [LA.Vector Double]
                             , _constr  :: Double
                             , _len     :: Int
                             , _penalty :: Double
                             }

-- | creates an unevaluated individual.
createIndividual :: TIR -> Individual
createIndividual tir = Individual tir [] [] 0.0 0 0.0

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

instance Eq Individual where
    t1 == t2 = penalizedFit t1 == penalizedFit t2 
instance Ord Individual where
    t1 <= t2 = penalizedFit t1 <= penalizedFit t2

instance NFData Individual where
  rnf _ = ()

instance Solution Individual where
  _getFitness = head . _fit
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
