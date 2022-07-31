{-|
Module      : MachineLearning.TIR.Random
Description : Functions to generate random expressions
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}

module MachineLearning.TIR.Random where 

import System.Random
import Control.Evolution
import MachineLearning.TIR
import MachineLearning.Utils.Config
import Control.Monad.State.Strict
import Data.List                                 (delete)

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
