{-|
Module      : MachineLearning.Utils.Report
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Report generation.
-}
module MachineLearning.Utils.Report where

import           System.Directory
import           System.IO
import           System.Clock
import           Data.Maybe                  (fromMaybe)
import           Data.List                   (intercalate, foldl')
import           Data.List.Split             (splitOn)
import           Data.Int                    (Int64)
import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import qualified Numeric.LinearAlgebra as LA

import Control.Evolution (Population)
import MachineLearning.Model.Measure                         (Measure(..))
import MachineLearning.TIR                         (Individual(..), assembleTree, replaceConsts)
import Data.SRTree.Print                            (showDefault, showPython)
import MachineLearning.Utils.Config                          (Config(..), Output(..), IOCfg(..), Task(..), AlgorithmCfg(..), getLogType, getMeasures)
import Data.SRTree (SRTree(..))

-- | Creates a file if it does not exist
createIfDoesNotExist :: FilePath -> IO Handle
createIfDoesNotExist fname = do
  isCreated <- doesFileExist fname
  if   isCreated
  then openFile fname AppendMode
  else openFile fname WriteMode

-- | writes the final population (GP), pareto front (MOO), distinct solutions (FitShare)
writeFront :: Config -> Population Individual -> IO () 
writeFront cfg front = do 
    let dirname = case getLogType cfg of 
                    Screen         -> "results/"
                    PartialLog dir -> dir 
                    EvoLog dir     -> dir
        frontFname = dirname ++ "/front.csv"
        printIndividual h x = hPutStrLn h $ show (_fit x) <> " " <> showPython (showTree x)
    h <- openFile frontFname WriteMode
    mapM_ (printIndividual h) front 
    hClose h 
  where
    showTree x = assembleTree bias $ replaceConsts tir consts 
        where 
            tir    = _chromo x 
            ws     = _weights x
            bias   = V.head $ VS.convert $ head ws
            consts = V.tail $ VS.convert $ head ws 

-- | writes all the stats from the final champion solution
writeChampionStats :: Config -> (Individual -> Maybe [Double]) -> Int64 -> Individual -> IO ()
writeChampionStats cfg fitTest totTime champion = do
  let dirname = case getLogType cfg of
                  Screen         -> "results/"
                  PartialLog dir -> dir
                  EvoLog dir     -> dir
      statsFname = dirname ++ "/stats.json"
      exprFname  = dirname ++ "/exprs.csv"
      cfgFname   = dirname ++ "/parameters.csv"
      task       = _task $ _algorithmCfg cfg
  createDirectoryIfMissing True dirname
  writeCfg cfgFname cfg
  writeExprs task exprFname champion
  writeStats statsFname fitTest (getMeasures cfg) totTime champion

-- | writes the stats to a file
writeStats :: FilePath -> (Individual -> Maybe [Double]) -> [Measure] -> Int64 -> Individual -> IO ()
writeStats statsFname fitTest measures totTime champion = do
    h <- createIfDoesNotExist statsFname
    hPutStrLn h $ "{" ++ intercalate "," json ++ "}"
    hClose h
  where
    mNames     = map _name measures
    trainNames = map (++"_train") mNames
    trainErrors = map show $ _accs champion
    testErrors  = map show $ fromMaybe nans $ fitTest champion
    testNames  = map (++"_test")  mNames
    fields     = "tot_time" : (trainNames ++ testNames)
    nans       = replicate (length testNames) (1/0)
    errors     = show totTime : (trainErrors ++ testErrors)
    toJson k v = show k ++ " : " ++ v
    json       = zipWith toJson fields errors

-- | creates a report file
writeCfg :: FilePath -> Config -> IO ()
writeCfg cfgFname cfg = do
  h <- createIfDoesNotExist cfgFname
  hPrint h cfg
  hClose h

-- | pretty write the expressions
writeExprs :: Task -> FilePath -> Individual -> IO ()
writeExprs task exprFname champion = do
    h <- createIfDoesNotExist exprFname
    hPutStrLn h $ unlines logs
    hPutStrLn h "======================================"
    hClose h
  where
    tir    = _chromo champion
    trees  = map getTree ws
    ws     = _weights champion
    bias   = V.head $ VS.convert $ head ws
    logs   = zipWith (\t w -> "\"" ++ showDefault t ++ "\",\"" ++ show w ++ "\",\"" ++ showPython t ++ "\"") trees ws 

    getTree :: LA.Vector Double -> SRTree Int Double
    getTree w = let bias   = V.head $ VS.convert w
                    consts = V.tail $ VS.convert w
                    sigm z = 1 / (1+exp(-z))
                in  case task of 
                      Classification _ -> sigm $ assembleTree bias $ replaceConsts tir consts
                      ClassMult      _ -> sigm $ assembleTree bias $ replaceConsts tir consts
                      _                -> assembleTree bias $ replaceConsts tir consts

-- | creates a log of the evolution process
evoLog :: Handle -> (Individual -> Maybe [Double]) -> Population Individual -> IO ()
evoLog h fitness pop = do
  let fitTrain   = V.toList $ V.map (head._fit) pop
      fitTest    = V.toList $ V.map (replaceWithNan . fitness) pop
      statsTrain = getStats fitTrain
      statsTest  = getStats fitTest
  hPutStrLn h $ statsToStr statsTrain ++ "," ++ statsToStr statsTest

getStats :: [Double] -> [Double]
getStats = postAgg . foldl' aggregate []

statsToStr :: Show a => [a] -> String
statsToStr = intercalate "," . map show

replaceWithNan :: Maybe [Double] -> Double
replaceWithNan Nothing      = 1/0
replaceWithNan (Just [])    = 1/0
replaceWithNan (Just (x:_)) = x

-- | Opens the first file available in the format "name.{i}.csv"
-- where 'i' follows a sequence from 0 onward.
openNext :: String -> IO Handle
openNext fname = go [fname ++ "." ++ show n ++ ".csv" | n <- [0 :: Int ..]]
  where
    -- this is a partial function applied to an infinite list
    -- so, what harm can it do?
    go []       = error "end of inifinity stream"
    go (fn:fns) = do b <- doesFileExist fn
                     if b
                        then go fns
                        else openFile fn WriteMode

postAgg :: [Double] -> [Double]
postAgg [best, worst, tot, count] = [best, worst, tot/count]
postAgg _ = error "wrong parameters count"

-- | aggregates the best, worst and average solutions
aggregate :: [Double] -> Double -> [Double]
aggregate [] train = [train,train,train,1]
aggregate [best, worst, tot, count] train = [min best train, max worst train, tot+train, count+1]
aggregate _ _ = error "wrong parameters count in aggregate"

-- | creates a log name instead of overwritting last one
createLogName :: Config -> FilePath
createLogName cfg = fileDir ++ "/" ++ dataset ++ "_evo"
  where
    fileDir = case getLogType cfg of
                EvoLog dir -> dir
                _           -> ""
    dataset = last $ splitOn "/" $ (_trainFilename . _ioCfg) cfg

-- | creates an IO logger function
makeLogger :: Config -> (Individual -> Maybe [Double]) -> IO (Population Individual -> IO (), Maybe Handle)
makeLogger cfg fitTest = case getLogType cfg of
                           EvoLog dir -> do createDirectoryIfMissing True dir
                                            h <- openNext (createLogName cfg)
                                            hPutStrLn h "bestTrain,worstTrain,avgTrain,bestTest,worstTest,avgTest"
                                            return (evoLog h fitTest, Just h)
                           _          -> return (\_ -> return (), Nothing)

-- | closes a file
closeIfJust :: Maybe Handle -> IO ()
closeIfJust Nothing  = return ()
closeIfJust (Just h) = hClose h
