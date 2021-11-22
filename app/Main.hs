module Main where

import MachineLearning.Utils.Config
import MachineLearning.Utils.Report
import MachineLearning.Utils.Data
import MachineLearning.TIR
import MachineLearning.TIR.Crossover
import MachineLearning.TIR.Mutation
import MachineLearning.TIR.Selection
import MachineLearning.Model.Fitness
import MachineLearning.Model.Measure
import Control.Evolution
import Data.SRTree
import Data.SRTree.Print

import System.Environment (getArgs)
import Control.Monad.State.Strict
import Data.Bifunctor             (second)
import Data.List.Split            (splitOn)
import Data.Maybe                 (fromJust)
import Numeric.LinearAlgebra      (size)
import System.Random              (mkStdGen, getStdGen)
import System.IO                  (Handle)
import System.Clock               (Clock(..), getTime, sec)
import Numeric.Interval
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector  as V

import Prelude hiding (null)

filterImage :: Interval Double -> [Function] -> [Function]
filterImage image = filter (isNotNullNaNInf . (`evalFun` image) . inverseFunc)
{-# INLINE filterImage #-}

isNotNullNaNInf :: Interval Double -> Bool
isNotNullNaNInf xs = not (null xs || isNaN xl || isNaN xu || isInfinite xl || isInfinite xu)
  where
    xl = inf xs
    xu = sup xs
{-# INLINE isNotNullNaNInf #-}

parseCLI :: [String] -> IO ()
parseCLI [expminP, expmaxP, tfuncsP, ytfuncsP, errorMetric, nGensP, nPopP, pcP, pmP, seedP, penalty, trainname] = do
  let mutCfg = dfltMutCfg { _kRange = (read expminP, read expmaxP)
                          , _yfuns  = map read $ splitOn "," ytfuncsP
                          , _funs   = map read $ splitOn "," tfuncsP
                          , _vars   = []
                          }
      algCfg = dfltAlgCfg { _gens     = read nGensP
                          , _nPop     = read nPopP
                          , _pm       = read pmP
                          , _pc       = read pcP
                          , _seed     = let s = read seedP in if s < 0 then Nothing else Just s
                          , _measures = [toMeasure errorMetric]
                          }
      ioCfg = IOCfg trainname trainname Screen
      pn    = read penalty
      cnst  = if pn == 0.0 then dfltCnstrCfg  else dfltCnstrCfg{ _penaltyType  = Len pn }
      cfg   = Conf mutCfg ioCfg algCfg cnst
  (champion, _, _) <- runGP cfg 
  let bias = V.head $ VS.convert $ head $ _weights champion
  print $ (showPython . assembleTree bias . _chromo) champion <> ";" <> (show . _len) champion <> ";" <> (show . _fit) champion 

parseCLI _ = putStrLn "Usage: ./tir cli expmin expmax tfuncs ytfuncs errorMetric nGens nPop pc pm seed penalty trainname"

runWithCfg :: [FilePath] -> IO ()
runWithCfg [fname] = do 
  cfg                         <- readConfig fname   
  t0                          <- getTime Realtime
  (champion, mh, fitnessTest) <- runGP cfg  
  putStrLn "Best expression (training set):\n"
  putStrLn $ prettyPrintsolution champion
  t1 <- getTime Realtime
  putStr "Fitness on the test set: "
  print $ (head . fromJust . fitnessTest) champion
  closeIfJust mh
  putStrLn $ "Total time: " ++ show (sec t1 - sec t0) ++ " secs."
  writeChampionStats cfg fitnessTest (sec t1 - sec t0) champion
runWithCfg _ = putStrLn "Usage: ./tir config filename"
        
runGP :: Config -> IO (Individual, Maybe Handle, Individual -> Maybe [Double])
runGP cfg@(Conf mutCfg _ algCfg cnstCfg) = do
  g <- case (_seed . _algorithmCfg) cfg of
         Nothing -> getStdGen
         Just x  -> return $ mkStdGen x
  (train, val, alldata, test, domains, image, nvars) <- processData cfg
  let
      budget       = max 5 $ min 15 $ size (snd train) `div` 10
      mutCfg'      = mutCfg{ _yfuns = filterImage image (_yfuns mutCfg), _vars = [0 .. nvars-1], _budget=budget }
      task         = _task algCfg
      measures     = _measures algCfg
      penalty      = _penaltyType cnstCfg
      cnstr        = const 0.0 -- fromShapes (_shapes cnstCfg) (_domains cnstCfg)
      fitnessTrain = evalTrain task False measures cnstr penalty domains (fst train) (snd train) (fst val) (snd val)
      fitnessAll   = evalTrain task True measures cnstr penalty domains (fst alldata) (snd alldata) (fst alldata) (snd alldata)
      fitnessTest  = evalTest task measures (fst test) (snd test)
      
      myCX OnePoint  = onepoint
      myCX CustomCX1 = uniformCx
      myMut GroupMutation = multiMut mutCfg'
      myRep Generational = generational
      mySelect (Tournament n) = tournament n
      myFilter _ = id
      myCreate = createIndividual <$> randomTIR mutCfg'
      -- myFitness x = liftIO (print (_chromo x)) >> liftIO (print (tirToMatrix (fst train) $ selectValidTerms (_chromo x) domains)) >> pure (fitnessTrain x)
      myFitness = pure . fitnessTrain

      interpret :: Interpreter Individual
      interpret = Funs myCX myMut myRep mySelect myFilter myCreate myFitness
      
      gp           = Reproduce Generational 
                       All End 
                       All (Cross OnePoint 2 (_pc algCfg) (Tournament 2)
                             (Mutate GroupMutation (_pm algCfg) End))
                                                      
      alg          = case _algorithm algCfg of
                       GA -> gp
                       FI -> undefined
  (logger, mh)  <- makeLogger cfg fitnessTest
  (_, champion) <- runEvolution (_gens algCfg) (_nPop algCfg) logger alg g interpret 
  return (fitnessAll champion, mh, fitnessTest)

main :: IO ()
main = do
  args <- getArgs  
  case args of
    ("cli":opts)    -> parseCLI opts
    ("config":opts) -> runWithCfg opts
    _               -> putStrLn "Usage: ./tir {cli/config} args"
