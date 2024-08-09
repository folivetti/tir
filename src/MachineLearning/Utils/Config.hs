{-# language OverloadedStrings #-}
{-|
Module      : MachineLearning.Utils.Config
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Configuration parsing and report generation.
-}
module MachineLearning.Utils.Config where

import Data.Ini.Config         (IniParser, readable, parseIniFile, section, fieldOf)
import Data.Text        hiding (map)
import Data.Text.IO            (readFile)
import Prelude          hiding (readFile)

-- import Constraints.Shape       (Shape(..), Domains)
import MachineLearning.Model.Measure           (Measure, toMeasure, Fitness(..))
import Data.SRTree                    (Function(..))
import Algorithm.ShapeConstraint

allFunctions = [Id .. ]

-- | Task can be Regression, Classification and One-vs-All Classification
data Task = Regression | RegressionNL Int | Classification Int | ClassMult Int
         deriving (Eq, Read, Show)

-- | Current algorithm implementation are traditional Evolutionary (GPTIR) and 
-- Feasible-Infeasible two-population for shape-constraint (SCTIR).
data Algorithm = GPTIR | SCTIR | MOO | FS deriving (Eq, Read, Show)

-- | Type of penalty function
data Penalty = NoPenalty | Len Double | Shape Double deriving (Show, Read)

-- | Output configuration 
data Output = Screen | PartialLog String | EvoLog String deriving (Read, Show)

-- | Configuration data
data Config = Conf { _mutationCfg   :: MutationCfg
                   , _ioCfg         :: IOCfg
                   , _algorithmCfg  :: AlgorithmCfg
                   , _constraintCfg :: ConstraintCfg
                   } deriving (Show)
                   
-- | Mutation config 
data MutationCfg = MutCfg { _kRange :: (Int, Int)
                          , _funs   :: [Function]
                          , _yfuns  :: [Function]
                          , _vars   :: [Int]
                          , _budget :: Int
                          } deriving (Show, Read)

dfltMutCfg :: MutationCfg
dfltMutCfg = MutCfg (-5, 5) allFunctions allFunctions [] 0

-- | Dataset and logging configs
data IOCfg = IOCfg { _trainFilename :: String
                   , _testFilename  :: String
                   , _logType       :: Output
                   } deriving (Show, Read)
                   
-- | Algorithm configuration
data AlgorithmCfg = AlgCfg { _algorithm :: Algorithm
                           , _task      :: Task
                           , _gens      :: Int
                           , _maxTime   :: Integer
                           , _nPop      :: Int
                           , _pm        :: Double
                           , _pc        :: Double
                           , _seed      :: Maybe Int
                           , _fitness   :: [Fitness]
                           , _measures  :: [Measure]
                           } deriving (Show)

dfltAlgCfg :: AlgorithmCfg
dfltAlgCfg = AlgCfg GPTIR Regression 100 10000000 100 0.25 1.0 Nothing [ExprMeasure "RMSE"] [toMeasure "RMSE"]

data ConstraintCfg = CnsCfg { _penaltyType :: Penalty
                            , _shapes      :: [Shape] 
                            , _domains     :: [(Double, Double)]
                            , _evaluator   :: Maybe Evaluator 
                            } deriving (Show, Read)

dfltCnstrCfg :: ConstraintCfg
dfltCnstrCfg = CnsCfg NoPenalty [] [] Nothing

getLogType :: Config -> Output
getLogType = _logType . _ioCfg

getSeed :: Config -> Maybe Int
getSeed = _seed . _algorithmCfg

getTask :: Config -> Task
getTask = _task . _algorithmCfg

getNPop :: Config -> Int
getNPop = _nPop . _algorithmCfg

getNGens :: Config -> Int
getNGens = _gens . _algorithmCfg

getTrainName, getTestName :: Config -> String
getTrainName = _trainFilename . _ioCfg
getTestName  = _testFilename  . _ioCfg

getDomains :: Config -> [(Double, Double)]
getDomains = _domains . _constraintCfg

getImage :: Config -> Maybe (Double, Double)
getImage = const Nothing -- findImg . _shapes . _constraintCfg
--  where
--    findImg []                 = Nothing
--    findImg (Image (lo, hi):_) = Just (lo, hi)
--    findImg (_:xs)             = findImg xs

getMeasures :: Config -> [Measure]
getMeasures = _measures . _algorithmCfg

getShapes :: Config -> [Shape]
getShapes = _shapes . _constraintCfg

getPenalty :: Config -> Penalty
getPenalty = _penaltyType . _constraintCfg

readConfig :: String -> IO Config
readConfig fname = do content <- readFile fname
                      case parseIniFile content parseConfig of
                        Left e    -> error e
                        Right cfg -> return cfg

-- | Read the config file and run the algorithm.
parseConfig :: IniParser Config
parseConfig = do
  mutCfg <- section "Mutation" $ do
    krange  <- fieldOf "krange" readable
    tfuncs  <- fieldOf "transfunctions" readable
    ytfuncs <- fieldOf "Ytransfunctions" readable
    return $ MutCfg krange tfuncs ytfuncs [] 0
  ioCfg <- section "IO" $ do
    trainname <- fieldOf "train" readable
    testname  <- fieldOf "test" readable
    logg      <- fieldOf "log" readable
    return $ IOCfg trainname testname logg 
  algCfg <- section "Algorithm" $ do
    alg  <- fieldOf "algorithm" readable
    task <- fieldOf "task" readable
    nGens <- fieldOf "ngens" readable
    maxTime <- fieldOf "maxTime" readable
    nPop <- fieldOf "npop" readable
    pm <- fieldOf "probmut" readable
    pc <- fieldOf "probcx" readable
    fit_mes <- fieldOf "fitness" readable
    perf_mes <- fieldOf "measures" readable
    seed <- fieldOf "seed" readable
    return $ AlgCfg alg task nGens maxTime nPop pm pc seed fit_mes $ map toMeasure perf_mes
  cnsCfg <- section "Constraints" $ do
    penalty <- fieldOf "penalty" readable
    shapes <- fieldOf "shapes" readable
    domains <- fieldOf "domains" readable
    evaluator <- fieldOf "evaluator" readable
    return $ CnsCfg penalty shapes domains evaluator 
  return $ Conf mutCfg ioCfg algCfg cnsCfg 
