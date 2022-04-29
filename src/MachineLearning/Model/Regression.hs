{-|
Module      : MachineLearning.Model.Regression
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Fitting functions for the coefficients.
-}
module MachineLearning.Model.Regression 
  ( fitTask
  , predictTask
  , evalPenalty
  , applyMeasures
  , nonlinearFit
  , tirToMatrix
  ) where

import Data.Bifunctor
import           Data.SRTree                   (evalFun, Function(..), OptIntPow(..), evalFun, inverseFunc)
import MachineLearning.TIR       (TIR(..),  Individual(..), Dataset, Constraint, assembleTree, replaceConsts)
import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import Data.Vector           ((!))
import           Data.List                     (nub)
import           Data.Vector.Storable          (Vector, splitAt)
import           Numeric.LinearAlgebra         ((<\>), Matrix)
import           Numeric.GSL.Fitting           (nlFitting, FittingMethod(..))
import           Prelude               hiding  (splitAt)

import qualified Numeric.LinearAlgebra                     as LA

import MachineLearning.TIR (Individual(..))
import MachineLearning.Utils.Config (Task(..), Penalty(..))
import MachineLearning.Model.Measure (Measure(..))

-- * IT specific stuff

createQ :: Vector Double -> LA.Matrix Double -> LA.Matrix Double
createQ ys = LA.fromColumns . map (*ys) . LA.toColumns
{-# INLINE createQ #-}

avg :: Vector Double -> Vector Double
avg ys = LA.fromList [LA.sumElements ys / fromIntegral (LA.size ys)]
{-# INLINE avg #-}

-- | transform a data matrix using a TIR expression. This function returns
-- a tuple with the transformed data of the numerator and denominator, respectivelly.
-- Each column of the transformed data represents one term of the TIR expression.
tirToMatrix :: Dataset Double -> TIR -> (LA.Matrix Double, LA.Matrix Double)
tirToMatrix xss (TIR _ p q) = bimap (LA.fromColumns . (bias:)) LA.fromColumns (p', q')
  where
    bias      = V.head xss
    xss'      = V.tail xss
    sigma2mtx = map (\(_, g, ps) -> evalFun g $ evalPi ps)
    evalPi    = foldr (\(ix, k) acc -> acc * (xss' ! ix ^^ k)) 1
    p'        = sigma2mtx p
    q'        = sigma2mtx q
{-# INLINE tirToMatrix #-}

-- | Fits a linear model using l2-penalty
ridge :: Matrix Double -> Vector Double -> Matrix Double
ridge a b = oA <\> oB
  where
   mu = 0.01

   a' = LA.tr a
   b' = LA.tr $ LA.asColumn b
   oA = (a' <> LA.tr a') + (mu * LA.ident (LA.rows a'))
   oB = a' <> LA.tr b'
{-# INLINE ridge #-}

-- | Predicts a linear model
predict :: Matrix Double -> Vector Double -> Vector Double 
predict xs w | LA.cols xs == LA.size w = xs LA.#> w
             | otherwise = error $ "predict: " ++ show (LA.size xs) ++ show (LA.size w)
{-# INLINE predict #-}

-- | Solve the OLS *zss*w = ys*
solveOLS :: Matrix Double -> Vector Double -> Vector Double 
solveOLS zss ys = zss <\> ys
{-# INLINE solveOLS #-}

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
--regress :: Matrix Double -> Vector Double -> [Vector Double]
regress :: TIR -> Dataset Double -> Vector Double -> [Vector Double]
regress tir xss ys = [ws]
  where
    (zssP, zssQ) = tirToMatrix xss tir
    ys'          = evalFun (inverseFunc $ _funY tir) ys
    zssQy        = createQ ys' zssQ
    zss          = if LA.cols zssQ >= 1
                       then zssP LA.||| negate zssQy 
                       else zssP
    ws             = if LA.cols zss == 1
                       then avg ys'
                       else solveOLS zss ys'
-- regress zss ys = [solveOLS zss ys]
{-# INLINE regress #-}

-- | Applies conjugate gradient for binary classification
--classify :: Matrix Double -> Vector Double -> [Vector Double]
classify :: Int -> TIR -> Dataset Double -> Vector Double -> [Vector Double]
classify niter tir xss ys = [ws]
  where
    ws           = nonlinearFit niter zssP zssQ ys sigmoid dsigmoid theta0
    theta0       = LA.konst 0 (LA.cols zssP + LA.cols zssQ)
    (zssP, zssQ) = tirToMatrix xss tir

-- | Applies conjugate gradient for one-vs-all classification
--classifyMult :: Matrix Double -> Vector Double -> [Vector Double]
classifyMult :: Int -> TIR -> Dataset Double -> Vector Double -> [Vector Double]
classifyMult niter tir xss ys = zipWith minimize yss theta0
  where
    numLabels    = length $ nub $ LA.toList ys
    yss          = map f [0 .. numLabels-1]
    minimize y t = nonlinearFit niter zssP zssQ y sigmoid dsigmoid t
    theta0       = replicate numLabels $ LA.konst 0 (LA.cols zssP + LA.cols zssQ)
    (zssP, zssQ) = tirToMatrix xss tir
    
    f sample = VS.map (\a -> if round a == sample then 1 else 0) ys

-- | chooses the appropriate fitting function
--fitTask :: Task -> Matrix Double -> Vector Double -> [Vector Double]
fitTask :: Task -> TIR -> Dataset Double -> Vector Double -> [Vector Double]
fitTask Regression             = regress
fitTask (RegressionNL niter)   = regressNL niter
fitTask (Classification niter) = classify niter
fitTask (ClassMult niter)      = classifyMult niter
{-# INLINE fitTask #-}

-- | sigmoid function for classification.
sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1+exp(-z))
{-# INLINE sigmoid #-}

-- | derivative sigmoid function for classification.
dsigmoid :: Floating a => a -> a
dsigmoid z = sigmoid z * (1 - sigmoid z)
{-# INLINE dsigmoid #-}

-- | chooses the appropriate prediction function
predictTask :: Task -> [Vector Double] -> Vector Double
predictTask _ []                   = error "predictTask: empty coefficients matrix"
predictTask Regression yss         = head yss
predictTask (RegressionNL _) yss   = head yss
predictTask (Classification _) yss = sigmoid $ head yss
predictTask (ClassMult _) yss      = LA.vector $ map (fromIntegral . LA.maxIndex) $ LA.toRows $ LA.fromColumns $ map sigmoid yss
{-# INLINE predictTask #-}

-- | evals the penalty function
evalPenalty :: Penalty -> Int -> Double -> Double
evalPenalty NoPenalty _   _   = 0.0
evalPenalty (Len c)   len _   = fromIntegral len * c
evalPenalty (Shape c) _   val = val*c
{-# INLINE evalPenalty #-}

-- | applies a list of performance measures
applyMeasures :: [Measure] -> Vector Double -> Vector Double -> [Double]
applyMeasures measures ys ysHat = map ((`uncurry` (ys, ysHat)) . _fun) measures
{-# INLINE applyMeasures #-}

regressNL :: Int -> TIR -> Dataset Double -> Vector Double -> [Vector Double]
regressNL niter tir xss ys = [ws]
  where
    ws           = nonlinearFit niter zssP zssQ ys f f' theta0
    f            = evalFun $ _funY tir
    f'           = derivative $ _funY tir
    theta0       = head $ regress tir xss ys
    (zssP, zssQ) = tirToMatrix xss tir

-- | Non-linear optimization using Levenberg-Marquardt method.
--nonlinearFit :: Monad m => Vector Double -> Matrix Double -> Matrix Double -> Vector Double -> m (Vector Double)
nonlinearFit :: Int
             -> Matrix Double 
             -> Matrix Double 
             -> Vector Double 
             -> (Vector Double -> Vector Double) 
             -> (Vector Double -> Vector Double) 
             -> Vector Double 
             -> Vector Double
nonlinearFit niter zssP zssQ ys f f' theta0 = fst $ nlFitting LevenbergMarquardtScaled 1e-6 1e-6 niter model' jacob' theta0
  where
    model'       = model f ys zssP zssQ
    jacob'       = jacob f' zssP zssQ    

-- | calculates the error given the parameter vector beta
model :: (Vector Double -> Vector Double) -> Vector Double -> Matrix Double -> Matrix Double -> Vector Double -> Vector Double
model f ys zssP zssQ beta 
  | LA.cols zssQ == 0 = f ysHat_P - ys
  | otherwise         = f ysHat - ys
  where
    (betaP, betaQ) = splitAt (LA.cols zssP) beta
    ysHat_P        = predict zssP betaP
    ysHat_Q        = if LA.cols zssQ == 0 then 0 else predict zssQ betaQ
    ysHat          = ysHat_P / (1 + ysHat_Q)

-- | calculates the Jacobian given the parameter vector beta. Doesn't support
-- the outer transformation function.
jacob :: (Vector Double -> Vector Double) -> Matrix Double -> Matrix Double -> Vector Double -> Matrix Double
jacob f zssP zssQ beta | LA.cols zssQ == 0 = zssP
                       | otherwise         = LA.fromColumns $ pjac <> qjac
  where
    (betaP, betaQ) = splitAt (LA.cols zssP) beta
    ysHat_P        = predict zssP betaP
    ysHat_Q        = predict zssQ betaQ
    ysHat          = f $ ysHat_P / (1 + ysHat_Q)
    pjac           = [ysHat * c / ysHat_Q | c <- LA.toColumns zssP]
    qjac           = [-ysHat * c * (ysHat_P / (1 + ysHat_Q)^2) | c <- LA.toColumns zssQ]

derivative :: (Eq val, Floating val) => Function -> val -> val
derivative Id      = const 1
derivative Abs     = \x -> x / abs x
derivative Sin     = cos
derivative Cos     = negate.sin
derivative Tan     = recip . (**2.0) . cos
derivative Sinh    = cosh
derivative Cosh    = sinh
derivative Tanh    = (1-) . (**2.0) . tanh
derivative ASin    = recip . sqrt . (1-) . (^2)
derivative ACos    = negate . recip . sqrt . (1-) . (^2)
derivative ATan    = recip . (1+) . (^2)
derivative ASinh   = recip . sqrt . (1+) . (^2)
derivative ACosh   = \x -> 1 / (sqrt (x-1) * sqrt (x+1))
derivative ATanh   = recip . (1-) . (^2)
derivative Sqrt    = recip . (2*) . sqrt
derivative Square  = (2*)
derivative Exp     = exp
derivative Log     = recip
{-# INLINE derivative #-}
-- (w1 * p1 + w2 * p2) / (1 + w3 * p3 + w4 * p4)
-- d/dw1 = p1 / (1 + w3 * p3 + w4 * p4)
-- d/dw2 = p2 / (1 + w3 * p3 + w4 * p4)
-- d/dw3 = -p3 * (w1 * p2 + w2 * p2) / (1 + w3 * p3 + w4 * p4)^2
{-
toEv :: SRTree Int Double -> (V.Vector Double -> Double)
toEv (Var !ix) = (`V.unsafeIndex` ix)
toEv (Const !val) = const val
toEv (Add !l !r) = jn (+) (toEv l) (toEv r)
toEv (Mul !l !r) = jn (*) (toEv l) (toEv r)
toEv (Fun Exp !t) = exp . toEv t
{-# INLINE toEv #-}

jn :: (Double -> Double -> Double) -> (V.Vector Double -> Double) -> (V.Vector Double -> Double) -> (V.Vector Double -> Double)
jn op f g = \x -> op (f x) (g x)
{-# INLINE jn #-}
-}
