module MachineLearning.Model.Regression 
  (fitTask
  , predictTask
  , evalPenalty
  , applyMeasures
  , nonlinearFit
  ) where

import           Data.List                     (nub)
import           Data.Vector.Storable          (Vector, splitAt)
import           Numeric.LinearAlgebra         ((<\>), Matrix)
import           Numeric.GSL.Fitting           (nlFitting, FittingMethod(..))
import           Prelude               hiding  (splitAt)

import qualified Numeric.LinearAlgebra                     as LA
import qualified MachineLearning.Classification.Binary     as BC
import qualified MachineLearning.Classification.OneVsAll   as OVA

import MachineLearning.TIR (Individual(..))
import MachineLearning.Utils.Config (Task(..), Penalty(..))
import MachineLearning.Model.Measure (Measure(..))

-- * IT specific stuff


-- | Predict a linear model using l2-penalty
ridge :: Matrix Double -> Vector Double -> Matrix Double
ridge a b = oA <\> oB
  where
   mu = 0.01

   a' = LA.tr a
   b' = LA.tr $ LA.asColumn b
   oA = (a' <> LA.tr a') + (mu * LA.ident (LA.rows a'))
   oB = a' <> LA.tr b'
{-# INLINE ridge #-}

predict :: Matrix Double -> Vector Double -> Vector Double 
predict xs w | LA.cols xs == LA.size w = xs LA.#> w
             | otherwise = error $ "predict: " ++ show (LA.size xs) ++ show (LA.size w)
{-# INLINE predict #-}

-- | Solve the OLS *zss*w = ys*
solveOLS :: Matrix Double -> Vector Double -> Vector Double 
solveOLS zss ys = zss <\> ys
{-# INLINE solveOLS #-}
-- solveOLS zss ys = LA.flatten $ LA.linearSolveLS zss $ LA.asColumn ys -- <\> ys

-- solveOLS zss ys = LA.flatten $ ridge zss ys

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
regress :: Matrix Double -> Vector Double -> [Vector Double]
regress zss ys = [solveOLS zss ys]
{-# INLINE regress #-}

classify :: Matrix Double -> Vector Double -> [Vector Double]
classify zss ys
    = let ws0     = LA.konst 0 (LA.cols zss)
          (ws, _) = BC.learn (BC.ConjugateGradientPR 0.1 0.1) 0.0001 500 BC.RegNone zss ys ws0
      in  [ws]

classifyMult :: Matrix Double -> Vector Double -> [Vector Double]
classifyMult zss ys
    = let ws0       = replicate numLabels $ LA.konst 0 (LA.cols zss)
          numLabels = length $ nub $ LA.toList ys
          (ws, _)   = OVA.learn (OVA.ConjugateGradientPR 0.1 0.1) 0.0001 500 OVA.RegNone numLabels zss ys ws0
      in  ws

-- | Fitness function for regression

fitTask :: Task -> Matrix Double -> Vector Double -> [Vector Double]
fitTask Regression     = regress
fitTask Classification = classify
fitTask ClassMult      = classifyMult
{-# INLINE fitTask #-}

sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1+exp(-z))
{-# INLINE sigmoid #-}

predictTask :: Task -> [Vector Double] -> Vector Double
predictTask _ []               = error "predictTask: empty coefficients matrix"
predictTask Regression yss     = head yss
predictTask Classification yss = sigmoid $ head yss
predictTask ClassMult yss      = LA.vector $ map (fromIntegral . LA.maxIndex) $ LA.toRows $ LA.fromColumns $ map sigmoid yss
{-# INLINE predictTask #-}

evalPenalty :: Penalty -> Int -> Double -> Double
evalPenalty NoPenalty _   _   = 0.0
evalPenalty (Len c)   len _   = fromIntegral len * c
evalPenalty (Shape c) _   val = val*c
{-# INLINE evalPenalty #-}

applyMeasures :: [Measure] -> Vector Double -> Vector Double -> [Double]
applyMeasures measures ys ysHat = map ((`uncurry` (ys, ysHat)) . _fun) measures
{-# INLINE applyMeasures #-}

nonlinearFit :: Monad m => Vector Double -> Matrix Double -> Matrix Double -> Vector Double -> m (Vector Double)
nonlinearFit ys_train zssP zssQ = return . fst . nlFitting LevenbergMarquardtScaled 1e-9 1e-9 200 model' jacob'
  where
    model' = model ys_train zssP zssQ
    jacob' = jacob zssP zssQ
    
model :: Vector Double -> Matrix Double -> Matrix Double -> Vector Double -> Vector Double
model ys_train zssP zssQ beta 
  | LA.cols zssQ == 0 = ysHat_P - ys_train
  | otherwise         = ysHat - ys_train
  where
    (betaP, betaQ) = splitAt (LA.cols zssP) beta
    ysHat_P        = predict zssP betaP
    ysHat_Q        = if LA.cols zssQ == 0 then 0 else predict zssQ betaQ
    ysHat          = ysHat_P / (1 + ysHat_Q)
  
jacob :: Matrix Double -> Matrix Double -> Vector Double -> Matrix Double
jacob zssP zssQ beta | LA.cols zssQ == 0 = zssP
                     | otherwise         = LA.fromColumns $ pjac <> qjac
  where
    (betaP, betaQ) = splitAt (LA.cols zssP) beta
    ysHat_P        = predict zssP betaP
    ysHat_Q        = predict zssQ betaQ
    ysHat          = ysHat_P / (1 + ysHat_Q)
    pjac           = [c / ysHat_Q | c <- LA.toColumns zssP]
    qjac           = [-c*(ysHat_P / ysHat_Q^2) | c <- LA.toColumns zssQ]
