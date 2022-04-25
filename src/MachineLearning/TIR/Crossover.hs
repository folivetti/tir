{-|
Module      : MachineLearning.TIR.Crossover
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Crossover operators.
-}
module MachineLearning.TIR.Crossover where

import MachineLearning.TIR
import Control.Evolution
import Control.Monad.State.Strict
import System.Random
import Data.List (nub, sort)

toss :: Rnd Bool 
toss = state random
{-# INLINE toss #-}

rndWithProb :: Double -> Rnd Bool
rndWithProb p = (< p) <$> state random
{-# INLINE rndWithProb #-}

choose :: a -> a -> Bool -> a
choose x y b = if b then x else y
{-# INLINE choose #-}

-- | One-point crossover
onepoint :: [Individual] -> Rnd Individual
onepoint (p1:p2:_) = do
  r <- randomRng (0, npc1 + nqc1)
  if r == 0
     then swapY
     else if r <= length pc1
            then swapP
            else swapQ
  where
    c1    = _chromo p1
    c2    = _chromo p2
    pc1   = clean $ _p c1
    qc1   = clean $ _q c1
    pc2   = clean $ _p c2
    qc2   = clean $ _q c2
    npc1  = length pc1
    nqc1  = length qc1
    npc2  = length pc2
    nqc2  = length qc2
    sortPi (x,y,z) = (x,y,sort z)
    clean = nub . map sortPi
    swapY = pure $ p1{ _chromo = c1{ _p=pc1, _q = qc2 }, _fit = [] }
    swapP = do ix <- randomRng (0, min npc1 npc2 - 1)
               let pc' = take ix pc1 <> drop ix pc2 
               pure $ p1{ _chromo = c1{ _p = pc', _q = qc1 }, _fit = [] }
    swapQ = do ix <- randomRng (0, min nqc1 nqc2 - 1)
               let qc' = take ix qc1 <> drop ix qc2 
               pure $ p1{ _chromo = c1{ _p = pc1, _q = qc' }, _fit = [] }
onepoint _         = error "Not enough individuals for onepoint crossover"
    
-- | Uniform crossover
uniformCx :: [Individual] -> Rnd Individual
uniformCx (p1:p2:_) = do
  let c1   = _chromo p1
      c2   = _chromo p2
      f1   = _getFitness p1
      f2   = _getFitness p2
      pc1  = nub $ map sortPi $ _p c1
      pc2  = nub $ map sortPi $ _p c2
      qc1  = nub $ map sortPi $ _q c1
      qc2  = nub $ map sortPi $ _q c2
      rnd1 = rndWithProb (f1/(f1+f2))

      rndChoice x y = choose x y <$> rnd1
      sortPi (x,y,z) = (x,y,sort z)
  g' <- rndChoice (_funY c1) (_funY c2)
  p' <- zipWithM rndChoice pc1 pc2
  q' <- zipWithM rndChoice qc1 qc2

  let c = TIR g' (nub p') (nub q')
  pure $ p1{ _chromo=c, _fit=[] }

uniformCx _ = error "Not enough individuals for uniform crossover"
