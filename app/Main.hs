{-# language TypeFamilies #-}
module Main where

import MachineLearning.Utils.Config
import MachineLearning.Utils.Report
import MachineLearning.Utils.Data ( processData, getData )
import MachineLearning.TIR
import MachineLearning.Model.Fitness ( evalTrain, evalTest )
import Control.Evolution
    ( runEvolution, Solution(_getFitness), Population, Evolution, Interpreter )
import Data.SRTree.Print ( showPython, showDefault )

import System.Environment (getArgs)
import Data.Maybe                 (fromJust)
import System.Random              (mkStdGen, getStdGen)
import System.IO                  (Handle)
import System.Clock               (Clock(..), getTime, sec)
import qualified Data.Vector  as V

import Data.List ( (++), map, (!!), head, intercalate ) 
import Data.Ord (compare)
import Data.Function (on)
import Control.Arrow ((&&&))
import Algorithm.ShapeConstraint (getViolationFun)

import Loading

runCLI :: Task -> [String] -> IO ()
runCLI task params = do
  (champion, _, _, front) <- runGP $ parseCfg task $ toParams params
  let best = intercalate ";" (info champion)
      hof  = map toDef $ V.toList front
  print . unlines $ best : toDef champion : hof
    where
      toPy c     = showPython . getTree task (_chromo c)
      toDef c    = showDefault (getTree task (_chromo c) (head $ _weights c))
                 <> (';' : show (_len c)) <> (';' : show (_fit c)) <> (';' : showPython (getTree task (_chromo c) (head $ _weights c)))
      getTrees c = intercalate "#" $ map (toPy c) $ _weights c
      info c     = [getTrees c, (show . _len) c, (show . _fit) c]

runWithCfg :: [FilePath] -> IO ()
runWithCfg [fname] = do
  cfg <- readConfig fname
  t0  <- getTime Realtime
  (champion, mh, fitnessTest, front) <- runGP cfg

  putStrLn "Best expression (training set):\n"
  putStrLn $ prettyPrintsolution champion
  t1 <- getTime Realtime
  putStr "Fitness on the test set: "
  print $ (head . fromJust . fitnessTest) champion
  closeIfJust mh
  putStrLn $ "Total time: " ++ show (sec t1 - sec t0) ++ " secs."
  writeChampionStats cfg fitnessTest (sec t1 - sec t0) champion
  writeFront cfg front
runWithCfg _ = putStrLn "Usage: ./tir config filename"


getThings :: Config -> IO (Individual -> Individual, Individual -> Maybe [Double], Evolution Individual, Interpreter Individual)
getThings cfg@(Conf mutCfg _ algCfg cnstCfg) = do
  (train, val, alldata, test) <- getData cfg
  let 
    (domains, image, nvars, nsamples) = processData cfg train
    mutCfg'  = updateMutCfg mutCfg nsamples image nvars
    task     = _task algCfg

    mySample f = distSample (f nvars domains) 10
    interpret = createInterpreter mutCfg' fitnessTrain $ distFun task (mySample bandExt)
    alg = algBuilder (_algorithm algCfg) (_pc algCfg) (_pm algCfg)

    fitnessTrain = fitFun False train val
    fitnessAll   = fitFun True alldata alldata
    fitnessTest  = uncurry (evalTest (_task algCfg) measures) test
    fitFun b (x, y) (x', y') = evalTrain (_task algCfg) b (_fitness algCfg) measures cnstr penalty domains x y x' y'
    cnstr    = case _evaluator cnstCfg of
                 Nothing -> const 0.0
                 Just e  -> getViolationFun e (_shapes cnstCfg) (_domains cnstCfg)
    measures = _measures algCfg
    penalty  = _penaltyType cnstCfg
  pure(fitnessAll, fitnessTest, alg, interpret)

runGP :: Config -> IO (Individual, Maybe Handle, Individual -> Maybe [Double], Population Individual)
runGP cfg@(Conf mutCfg _ algCfg cnstCfg) = do
  g <- case (_seed . _algorithmCfg) cfg of
         Nothing -> getStdGen
         Just x  -> return $ mkStdGen x

  (fitnessAll, fitnessTest, alg, interpret) <- getThings cfg
  (logger, mh)  <- makeLogger cfg fitnessTest
  (_, champion, finalPop) <- runEvolution (_gens algCfg) (Just $ _maxTime algCfg) (_nPop algCfg) logger alg g interpret
  -- putStrLn $ "Avg. dist.: " <> show (avgDist finalPop)
  let champion' = if _algorithm algCfg == MOO
                     then bestTradeOff champion finalPop
                     else bestAcc champion finalPop
      front     = if _algorithm algCfg == FS
                     then V.fromList $ findFarthest 5 (champion : V.toList finalPop)
                     else V.cons champion finalPop

  return (fitnessAll champion', mh, fitnessTest, front)

bestAcc champion front = champion

bestTradeOff :: Individual -> V.Vector Individual -> Individual
bestTradeOff champion front = V.minimumBy cmpLen $ V.filter ((<=thr) . head . _getFitness) front
  where
    thr      = getThr . head . _getFitness $ champion
    getThr x = if x < 0 then 0.95*x else 1.05*x

    errFun, lenFun :: Individual -> Double
    errFun   = head . _getFitness
    lenFun   = (!!1) . _getFitness
    cmpError = compare `on` errFun
    cmpLen   = compare `on` ((!!1) . _getFitness)

bestBic champion front = champion -- bic model  = let n = size (snd train) in n * ln(mse) + n(1 + ln(2pi)) + ln(n)k

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("regress":opts)    -> runCLI Regression opts
    ("regressNL":opts)  -> runCLI (RegressionNL 0) opts
    ("class":opts)      -> runCLI (Classification 0) opts
    ("multiclass":opts) -> runCLI (ClassMult 0) opts
    ("config":opts)     -> runWithCfg opts
    _anyOther           -> putStrLn "Usage: ./tir {regress/regressNL/class/multiclass/config} args"
