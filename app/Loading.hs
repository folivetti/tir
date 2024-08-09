{-# language TypeFamilies #-}
module Loading where

import MachineLearning.Utils.Config
import MachineLearning.Utils.Report
import MachineLearning.Utils.Data
import MachineLearning.TIR
import MachineLearning.TIR.Random
import MachineLearning.TIR.Crossover
import MachineLearning.TIR.Mutation
import MachineLearning.Model.Fitness
import MachineLearning.Model.Measure
import Control.Evolution
import Data.SRTree
import Data.SRTree.Print

import System.Environment (getArgs)
import Control.Monad.State.Strict
import Data.Bifunctor             (second)
import Data.List.Split            (splitOn)
import Data.Maybe                 (fromJust, fromMaybe)
import Numeric.LinearAlgebra      (size)
import System.Random              (mkStdGen, getStdGen)
import System.IO                  (Handle)
import System.Clock               (Clock(..), getTime, sec)
import Numeric.ModalInterval
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector  as V
import qualified Numeric.LinearAlgebra as LA

import Data.List (intercalate, transpose, nubBy, sortBy, foldl', minimumBy, maximumBy)
import Data.Ord (compare)
import Data.Function (on)
import Control.Arrow ((&&&))
import Prelude hiding (null)
import Algorithm.ShapeConstraint (getViolationFun)

-- Evolutionary definitions
--
instance EvoClass Individual where
  data Crossover Individual = OnePoint | UniformCX deriving (Show, Read)
  data Mutation  Individual = GroupMutation deriving (Show, Read)

cross, mut :: Double -> Op Individual
cross pc = Cross OnePoint 2 pc (Tournament 2)
mut      = Mutate GroupMutation

gp, fi, moo, fs :: Double -> Double -> Evolution Individual
gp pc pm = Reproduce Generational 
             [cross pc :> mut pm :> Done]
fi pc pm = Reproduce Merge
             [ With Feasible :> cross pc :> mut pm :> Done
             , With Infeasible :> cross pc :> mut pm :> Done
             ]
moo pc pm = Reproduce NonDominated 
              [Done, cross pc :> mut pm :> Done]
fs pc pm  = Reproduce (Probabilistic (FitShare 0.1 2)) 
              [Done, cross pc :> mut pm :> Done]

createInterpreter :: MutationCfg -> (Individual -> Individual) -> (Individual -> Individual -> Double) -> Interpreter Individual
createInterpreter mutCfg fitness myDist = Funs myCX myMut myCreate myFitness
  where
    myCX OnePoint  = onepoint
    myCX UniformCX = uniformCx
    myMut GroupMutation = multiMut mutCfg
    myCreate = createIndividual myDist <$> randomTIR mutCfg
    myFitness = pure . fitness

algBuilder alg = case alg of
                   GPTIR -> gp
                   SCTIR -> fi
                   MOO   -> moo
                   FS    -> fs


-- * Mutation config processing
filterImage :: Kaucher Double -> [Function] -> [Function]
filterImage image = filter (isNotNullNaNInf . (`evalFun` image) . inverseFunc)
{-# INLINE filterImage #-}

isNotNullNaNInf :: Kaucher Double -> Bool
isNotNullNaNInf xs = not (isEmpty xs || isNaN xl || isNaN xu || isInfinite xl || isInfinite xu)
  where
    xl = fromMaybe (-1/0) $ inf xs
    xu = fromMaybe (1/0) $ sup xs
{-# INLINE isNotNullNaNInf #-}

updateMutCfg :: MutationCfg -> Int -> Kaucher Double -> Int -> MutationCfg
updateMutCfg cfg nsamples img nvars =
    let budget = max 5 $ min 15 $ nsamples `div` 10
        in cfg{ _yfuns = filterImage img (_yfuns cfg)
              , _vars = [0 .. nvars-1]
              , _budget = budget
              }

-- * Config parsing
parseMutation :: [String] -> MutationCfg
parseMutation [expminP, expmaxP, tfuncsP, ytfuncsP] =
    dfltMutCfg { _kRange = (read expminP, read expmaxP)
               , _yfuns  = map read $ splitOn "," ytfuncsP
               , _funs   = map read $ splitOn "," tfuncsP
               , _vars   = []
               }
parseMutation xs = error $ "Not enough parameters for parseMutation: " <> show xs

parseAlgorithm :: Task -> [String] -> AlgorithmCfg
parseAlgorithm task [errorMetric, nGensP, maxTime, nPopP, pcP, pmP, seedP, alg] =
    dfltAlgCfg { _gens      = read nGensP
               , _maxTime   = read maxTime
               , _nPop      = read nPopP
               , _algorithm = read alg
               , _pm        = read pmP
               , _pc        = read pcP
               , _seed      = let s = read seedP in if s < 0 then Nothing else Just s
               , _fitness   = [ExprMeasure errorMetric, ExprLen]
               , _measures  = [toMeasure errorMetric]
               , _task = task
               }
parseAlgorithm _ xs = error $ "Not enough parameters for parseAlgorithm: " <> show xs

parseCfg :: Task -> [[String]] -> Config
parseCfg task [mutPars, algPars, [trainname, penalty, niter]] =
    let mutCfg = parseMutation mutPars
        task' = case task of
                  Regression -> Regression
                  RegressionNL _ -> RegressionNL $ read niter
                  Classification _ -> Classification $ read niter
                  ClassMult _ -> ClassMult $ read niter
        algCfg = parseAlgorithm task' algPars 
        ioCfg = IOCfg trainname trainname Screen
        pn    = read penalty
        cnst  = if pn == 0.0 then dfltCnstrCfg  else dfltCnstrCfg{ _penaltyType  = Len pn }
     in Conf mutCfg ioCfg algCfg cnst
parseCfg _ _ = error "Usage: ./tir cli expmin expmax tfuncs ytfuncs errorMetric nGens nPop pc pm seed penalty alg trainname maxTime"

toParams :: [String] -> [[String]]
toParams [expminP, expmaxP, tfuncsP, ytfuncsP,errorMetric, nGensP, nPopP, pcP, pmP, seedP, penalty, niter, alg, trainname, maxTime] =
    [[expminP, expmaxP, tfuncsP, ytfuncsP], [errorMetric, nGensP, maxTime, nPopP, pcP, pmP, seedP, alg], [trainname, penalty, niter]]
toParams _ = error "Usage: ./tir cli expmin expmax tfuncs ytfuncs errorMetric nGens nPop pc pm seed penalty alg trainname maxTime"

-- * Tree manipulation functions
getTree :: Task -> TIR -> LA.Vector Double -> SRTree Int Double
getTree task tir w =
  let bias   = V.head $ VS.convert w
      consts = V.tail $ VS.convert w
      sigm z = 1 / (1+exp(-z))
  in  case task of
        Classification _ -> sigm $ assembleTree bias $ replaceConsts tir consts
        ClassMult      _ -> sigm $ assembleTree bias $ replaceConsts tir consts
        _regression      -> assembleTree bias $ replaceConsts tir consts

-- * Distance for fitness sharing

-- find farthest points heuristic
-- discrete p-dispersion (maxmin) problem
-- NP
-- Heuristic and Special Case Algorithms for Dispersion Problems (Ravi, 1994)
-- The maximal-dispersion problem Get access (White, 1991)
-- TODO: make it faster with zippers
findFarthest _ []  = []
findFarthest 0 pop = []
findFarthest 1 pop = [minimumBy (compare `on` (head . _getFitness)) pop]
findFarthest n pop = findFarthest' (n - 1) p
    where
        p = findFarthest 1 pop
        findFarthest' 0 p = p
        findFarthest' m p = let p' = maximumBy (compare `on` minDist p) pop : p
                            in  findFarthest' (m-1) p'
        minDist xs x = minimum [_distFun x x y | y <- xs]

avgDist :: V.Vector Individual -> Double
avgDist pop = sum dists / fromIntegral (length dists)
  where
    enumerate = zip [0..]
    pop'      = enumerate $ V.toList pop
    dists     = [ _distFun x x y | (i, x) <- pop'
                                 , (j, y) <- pop'
                                 , i < j
                ]
-- dissimilarity function
toTheLeft, toTheRight :: Double -> Double
toTheLeft x | x < 0 = 1.3*x
            | otherwise = 0.7*x
toTheRight x | x < 0 = 0.7*x
             | otherwise = 1.3*x

getNewLo, getNewHi :: Maybe Double -> (Double, Double)
getNewLo (Just x) = (toTheLeft x, x)
getNewLo Nothing = error "getNewLo"
getNewHi (Just x) = (x, toTheRight x)
getNewHi Nothing = error "getNewHi"

genListPoints :: Int -> Int -> (Double, Double) -> [Double]
genListPoints nvars n (lo, hi) = [lo, lo + step .. hi]
    where 
      step   = (hi - lo) / (n' ** (1 / nvars'))
      n'     = fromIntegral n
      nvars' = fromIntegral nvars

bandExt, bandInt, bandBoth :: Int -> V.Vector (Kaucher Double) -> Int -> [[Double]]
bandExt nvars domains n = map (f . getLoHi) $ V.toList domains
    where
        getLoHi  = getNewLo . inf &&& getNewHi . sup
        half     = n `div` 2
        f (x, y) = genListPoints nvars half x
                <> genListPoints nvars half y

bandInt nvars domains n = map (f . getLoHi) $ V.toList domains
    where
        getLoHi  = fromJust . inf &&& fromJust . sup
        f xy = genListPoints nvars n xy
           
bandBoth nvars domains n = zipWith (<>) (bandInt nvars domains half) (bandExt nvars domains half)
    where half = n `div` 2

distSample :: (Int -> [[Double]]) -> Int -> V.Vector (LA.Vector Double)
distSample band n = V.fromList . map LA.fromList . transpose . map (1:) . take n $ sequence (band n)

distFun :: Task -> Dataset Double -> Individual -> Individual -> Double
distFun task pts x1 x2 =  min 1 $ mse / (v * n)
  where
        p1 = LA.toList $ evalToTarget task pts x1
        p2 = LA.toList $ evalToTarget task pts x2
        n  = fromIntegral $ length p1
        diff :: (Double, Double) -> Double
        diff (a, b) | (isNaN a || isInfinite a) || (isNaN b || isInfinite b) = 0
                    | otherwise = (a - b)*(a - b)
        mse :: Double
        mse = foldr (\x acc -> acc + diff x) 0 $ zip p1 p2
        v1 = var p1
        v2 = var p2
        v  = 1e-6 + max v1 v2

        var :: [Double] -> Double
        var xs = sum' / n
          where
            mu   = sum xs / n
            sum' = foldr (\x s -> s + (x-mu)^(2 :: Int)) 0 xs

