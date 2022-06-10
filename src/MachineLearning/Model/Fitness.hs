{-# language FlexibleInstances #-}
{-|
Module      : MachineLearning.Model.Fitness 
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the functions that calculates the coefficients,
evaluate the fitness, penalty and constraints of a TIR expression.
-}
module MachineLearning.Model.Fitness 
  ( evalTrain
  , evalTest
  , selectValidTerms
  ) where

import Data.Bifunctor
import Data.Maybe            (fromJust)
import Data.Vector.Storable  (Vector)
import Data.Vector           ((!))
import Numeric.LinearAlgebra ((<\>))
import Numeric.ModalInterval      (Kaucher, inf, sup, width, singleton)
import qualified Numeric.ModalInterval as Interval 
import Control.Monad.Reader
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import Data.Maybe (fromMaybe)
import MachineLearning.Model.Measure       (Measure)
import MachineLearning.Model.Regression    (nonlinearFit, evalPenalty, fitTask, predictTask, applyMeasures)
import MachineLearning.TIR       (TIR(..),  Individual(..), Dataset, Constraint, assembleTree, replaceConsts)
import MachineLearning.Utils.Config       (Task(..), Penalty)
import Data.SRTree                (SRTree(..), Function, OptIntPow(..), evalTree, evalTreeMap, evalFun, inverseFunc, countNodes)

import Data.SRTree.Print

-- | removes invalid terms from the TIR expression. Invalid terms
-- are those that evaluate to `NaN` or `Infinite` within the
-- domains of each variable. The domains are either provided by
-- the configuration file or estimated using the training data.
selectValidTerms :: TIR -> V.Vector (Kaucher Double) -> TIR
selectValidTerms tir@(TIR _ p q) domains = tir{ _p=p', _q=q' }
  where
    p' = selectValid p
    q' = selectValid q
    
    selectValid = filter (\(_, g, ps) -> isValidInterval $ evalFun g (evalPi ps))
    evalPi      = foldr (\(ix, k) acc -> acc * (domains ! ix ^. k)) 1 
{-# INLINE selectValidTerms #-}

isValidInterval = not. isInvalidInterval
{-# INLINE isValidInterval #-}

isInvalidInterval :: Kaucher Double -> Bool                        
isInvalidInterval ys =  Interval.isEmpty ys 
                     || Interval.isInvalid ys
                     || isInfinite ys1 || isInfinite ys2 
                     || ys2 < ys1 
                     || abs ys1 >= 1e50 || abs ys2 >= 1e50
                     || isNaN ys1 || isNaN ys2
                     || width ys < 1e-8
  where
    ys1 = fromMaybe (-1/0) $ inf ys
    ys2 = fromMaybe (1/0) $ sup ys
{-# INLINE isInvalidInterval #-}            

-- | evaluates an individual first fitting the expression
-- with either OLS or a nonlinear optimization (not yet implemented) 
-- and calculating the fitness vector, constraints, penalty.
evalTrain :: Task                          -- ^ Regression or Classification task
          -> Bool                          -- ^ if it is a single (False) or MOO (True)
          -> Bool                          -- ^ if we are fitting the final best individual, in this case do not split the training data for validation
          -> [Measure]                     -- ^ list of performance measures to calculate
          -> Constraint                    -- ^ constraint function
          -> Penalty                       -- ^ penalty
          -> V.Vector (Kaucher Double)     -- ^ variable domains represented as a Kaucher Interval
          -> Dataset Double                -- ^ training data
          -> Vector Double                 -- ^ training target
          -> Dataset Double                -- ^ validation data
          -> Vector Double                 -- ^ validation target
          -> Individual 
          -> Individual
evalTrain task moo isRefit measures cnstrFun penalty domains xss_train ys_train xss_val ys_val sol
--  | LA.cols zss == 0                   = error "found"
--  | (not.null) (LA.find (\x -> isNaN x || isInfinite x) zss)  = error $ (show $ _chromo sol) <> show domains 
  | not isRefit && (not.null._fit) sol = sol
--  | LA.cols zssP == 0                  = sol { _fit = [1/0] }
  | otherwise                          = sol{ _chromo   = fitted
                                            , _fit      = fitness
                                            , _accs     = myMeasures
                                            , _weights  = ws
                                            , _constr   = cnst
                                            , _len      = len
                                            , _penalty  = pnlty 
                                            }
  where
    -- Fit the rational IT
    tir          = selectValidTerms (_chromo sol) domains
    ws           = fitTask task tir xss_train ys_train
    
    -- Validate (it should be applied to every different weights set)
    fitted         = replaceConsts tir . V.tail . VS.convert . head
                   $ ws
    fitness        = if moo then head myMeasures : [fromIntegral len] else [head myMeasures]
    myMeasures     = map nan2inf . fromJust . evalTest task measures xss_val ys_val
                   $ sol{ _chromo=fitted, _weights=ws }
    -- Length and constraint   
    tree           = assembleTree (V.head $ VS.convert $ head ws) fitted
    len            = countNodes tree
    cnst           = cnstrFun tree
    pnlty          = evalPenalty penalty len cnst


-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
evalTest :: Task -> [Measure] -> Dataset Double -> Vector Double -> Individual -> Maybe [Double]
evalTest task measures xss ys sol
  | null weights = Nothing
  | otherwise    = Just
                 $ applyMeasures measures ys
                 $ predictTask task
                 $ map (evalTIR xss' bias . replaceConsts tir . V.tail . VS.convert) weights
  where
    tir          = _chromo sol
    weights      = _weights sol
    bias         = V.head $ VS.convert $ head weights -- only works for regression
    -- nSamples     = VS.length $ V.head xss
    xss'         = V.tail xss
    -- treeEval t   = fromJust $ runReader (evalTreeMap (VS.replicate nSamples) t) (xss' V.!?)

evalTIR :: Dataset Double -> Double -> TIR -> LA.Vector Double
evalTIR xss bias (TIR g p q) = evalFun g ((LA.scalar bias + p') / (1 + q'))
  where
    p'     = foldr (\(w, h, ks) acc -> LA.scalar w * evalFun h (evalPi ks) + acc) 0 p
    q'     = foldr (\(w, h, ks) acc -> LA.scalar w * evalFun h (evalPi ks) + acc) 0 q
    evalPi = foldr (\(ix,k) acc -> acc * (xss ! ix)^^k) 1
{-# INLINE evalTIR #-}
    
--instance OptIntPow (LA.Vector Double) where
--  (^.) = (^^)


nan2inf :: RealFloat a => a -> a
nan2inf x | isNaN x = 1/0
          |otherwise = x
{-# INLINE nan2inf #-}
