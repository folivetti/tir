{-|
Module      : MachineLearning.Model.Measure 
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Performance measures for Regression and Classification.
-}
module MachineLearning.Model.Measure 
  ( Measure(..)
  , toMeasure
  , measureAll
  , _rmse
  )
  where

import Data.Semigroup (Sum(..))

import qualified Numeric.LinearAlgebra       as LA
import qualified Numeric.Morpheus.Statistics as Stat
import qualified Data.Vector.Storable        as V

type Vector = LA.Vector Double
-- * Performance measures

-- | A performance measure has a string name and a function that 
-- takes a vector of the true values, a vector of predict values
-- and returns a `Double`.
data Measure = Measure { _name :: String
                       , _fun  :: Vector -> Vector -> Double -- ^ true values -> predicted values -> measure
                       }

instance Show Measure where
  show (Measure n _) = n

-- | Mean for a vector of doubles
mean :: Vector -> Double
mean xs = V.sum xs / fromIntegral (V.length xs)
{-# INLINE mean #-}

-- | Variance for a vector of doubles
var :: Vector -> Double
var xs = sum' / fromIntegral (V.length xs)
  where
    mu   = mean xs
    sum' = V.foldl (\s x -> s + (x-mu)^(2 :: Int)) 0 xs
{-# INLINE var #-}

-- | generic mean error measure
meanError :: (Vector -> Vector) -- ^ a function to be applied to the error terms (abs, square,...)
          -> Vector             -- ^ target values
          -> Vector             -- ^ fitted values          
          -> Double
meanError op ys ysHat = mean $ op $ ysHat - ys
{-# INLINE meanError #-}

-- * Common error measures for regression:
-- MSE, MAE, RMSE, NMSE, r^2

-- | Mean Squared Error
mse :: Vector -> Vector -> Double
--mse           = meanError (^(2 :: Int))
mse ys ysHat = mean $ (ysHat - ys) ^(2 :: Int)
{-# INLINE mse #-}

-- | Mean Absolute Error
mae :: Vector -> Vector -> Double
mae ys ysHat = mean $ abs (ysHat - ys) -- meanError abs
{-# INLINE mae #-}

-- | Normalized Mean Squared Error
nmse :: Vector -> Vector -> Double
nmse ys ysHat = mse ysHat ys / var ys
{-# INLINE nmse #-}

-- | Root of the Mean Squared Error
rmse :: Vector -> Vector -> Double
rmse ys ysHat = sqrt $ mse ysHat ys
{-# INLINE rmse #-}

-- | negate R^2 - minimization metric
rSq :: Vector -> Vector -> Double
rSq ys ysHat = negate (1 - r/t)
  where
    ym      = Stat.mean ys
    t       = sumOfSq $ V.map (\yi -> yi - ym) ys
    r       = sumOfSq $ ys - ysHat
    sumOfSq = V.foldl (\s di -> s + di^(2 :: Int)) 0
{-# INLINE rSq #-}

-- * Regression measures
_rmse, _mae, _nmse, _r2 :: Measure
_rmse = Measure "RMSE" rmse
_mae  = Measure "MAE" mae
_nmse = Measure "NMSE" nmse
_r2   = Measure "R^2" rSq

-- * Classification measures
_accuracy,_recall,_precision,_f1,_logloss :: Measure
_accuracy  = Measure "Accuracy" accuracy
_recall    = Measure "Recall" recall
_precision = Measure "Precision" precision
_f1        = Measure "F1" f1
_logloss   = Measure "Log-Loss" logloss

-- | Accuracy: ratio of correct classification
accuracy :: Vector -> Vector -> Double
accuracy ys ysHat = -equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'
    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (yH, y)
      | yH == y   = (Sum 1, Sum 1)
      | otherwise = (Sum 0, Sum 1)

-- | Precision: ratio of correct positive classification
precision :: Vector -> Vector -> Double
precision ys ysHat = equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'
    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (1, 1)  = (Sum 1, Sum 1)
    cmp (1, 0)  = (Sum 0, Sum 1)
    cmp (_, _) = (Sum 0, Sum 0)

-- | Recall: ratio of retrieval of positive labels
recall :: Vector -> Vector -> Double
recall ys ysHat = equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'

    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (1, 1)  = (Sum 1, Sum 1)
    cmp (0, 1)  = (Sum 0, Sum 1)
    cmp (_, _) = (Sum 0, Sum 0)

-- | Harmonic average between Precision and Recall
f1 :: Vector -> Vector -> Double
f1 ys ysHat = 2*prec*rec/(prec+rec)
  where
    prec = precision ysHat ys
    rec  = recall ysHat ys

-- | LogLoss of a classifier that returns a probability.
logloss :: Vector -> Vector -> Double
logloss ys ysHat = mean $ -(ys * log ysHat' + (1 - ys)*log(1 - ysHat'))
  where
    ysHat' = LA.cmap (min (1.0 - 1e-15) . max 1e-15) ysHat



-- | List of all measures
measureAll :: [Measure]
measureAll = [_rmse, _mae, _nmse, _r2
             , _accuracy, _recall, _precision, _f1, _logloss
             ]

-- | Read a string into a measure
toMeasure :: String -> Measure
toMeasure input
  | null cmp  = error ("Invalid measure: " ++ input)
  | otherwise = (snd.head) cmp
  where
    cmp                       = filter fst $ map isThis measureAll
    isThis m@(Measure name _) = (name == input, m)
