{-# language TypeFamilies #-}
module Main where

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
import Data.Function (on) 

import Data.List (intercalate, transpose)
import Prelude hiding (null)
import Algorithm.ShapeConstraint (getViolationFun)

filterImage :: Kaucher Double -> [Function] -> [Function]
filterImage image = filter (isNotNullNaNInf . (`evalFun` image) . inverseFunc)
{-# INLINE filterImage #-}

isNotNullNaNInf :: Kaucher Double -> Bool
isNotNullNaNInf xs = not (isEmpty xs || isNaN xl || isNaN xu || isInfinite xl || isInfinite xu)
  where
    xl = fromMaybe (-1/0) $ inf xs
    xu = fromMaybe (1/0) $ sup xs
{-# INLINE isNotNullNaNInf #-}

instance EvoClass Individual where 
  data Crossover Individual = OnePoint | UniformCX deriving (Show, Read)
  data Mutation  Individual = GroupMutation deriving (Show, Read)

parseCLI :: Task -> [String] -> IO ()
parseCLI Regression [expminP, expmaxP, tfuncsP, ytfuncsP, errorMetric, nGensP, nPopP, pcP, pmP, seedP, penalty, alg, trainname] = do
  let mutCfg = dfltMutCfg { _kRange = (read expminP, read expmaxP)
                          , _yfuns  = map read $ splitOn "," ytfuncsP
                          , _funs   = map read $ splitOn "," tfuncsP
                          , _vars   = []
                          }
      algCfg = dfltAlgCfg { _gens     = read nGensP
                          , _nPop     = read nPopP
                          , _algorithm = read alg 
                          , _pm       = read pmP
                          , _pc       = read pcP
                          , _seed     = let s = read seedP in if s < 0 then Nothing else Just s
                          , _measures = [toMeasure errorMetric]
                          }
      ioCfg = IOCfg trainname trainname Screen
      pn    = read penalty
      cnst  = if pn == 0.0 then dfltCnstrCfg  else dfltCnstrCfg{ _penaltyType  = Len pn }
      cfg   = Conf mutCfg ioCfg algCfg cnst
  (champion, _, _, _) <- runGP cfg 
  let bias = V.head $ VS.convert $ head $ _weights champion
  print $ (showPython . assembleTree bias . _chromo) champion <> ";" <> (show . _len) champion <> ";" <> (show . _fit) champion 

parseCLI task [expminP, expmaxP, tfuncsP, ytfuncsP, errorMetric, nGensP, nPopP, pcP, pmP, seedP, penalty, niter, alg, trainname] = do
  let mutCfg = dfltMutCfg { _kRange = (read expminP, read expmaxP)
                          , _yfuns  = map read $ splitOn "," ytfuncsP
                          , _funs   = map read $ splitOn "," tfuncsP
                          , _vars   = []
                          }
      algCfg = dfltAlgCfg { _gens     = read nGensP
                          , _nPop     = read nPopP
                          , _pm       = read pmP
                          , _pc       = read pcP
                          , _algorithm = read alg
                          , _seed     = let s = read seedP in if s < 0 then Nothing else Just s
                          , _measures = [toMeasure errorMetric]
                          , _task     = case task of
                                          RegressionNL _ -> RegressionNL $ read niter
                                          Classification _ -> Classification $ read niter
                                          ClassMult _ -> ClassMult $ read niter
                          }
      ioCfg = IOCfg trainname trainname Screen
      pn    = read penalty
      cnst  = if pn == 0.0 then dfltCnstrCfg  else dfltCnstrCfg{ _penaltyType  = Len pn }
      cfg   = Conf mutCfg ioCfg algCfg cnst
  (champion, _, _, _) <- runGP cfg 
  
  let 
    tir    = _chromo champion
    trees  = intercalate "#" . map (showPython . getTree tir) $ _weights champion
  print $ trees <> ";" <> (show . _len) champion <> ";" <> (show . _fit) champion 
  where
    getTree :: TIR -> LA.Vector Double -> SRTree Int Double
    getTree tir w = let bias   = V.head $ VS.convert w
                        consts = V.tail $ VS.convert w
                        sigm z = 1 / (1+exp(-z))
                    
                in  case task of 
                      Classification _ -> sigm $ assembleTree bias $ replaceConsts tir consts
                      ClassMult      _ -> sigm $ assembleTree bias $ replaceConsts tir consts
                      _                -> assembleTree bias $ replaceConsts tir consts
                        
parseCLI _ _ = putStrLn "Usage: ./tir cli expmin expmax tfuncs ytfuncs errorMetric nGens nPop pc pm seed penalty alg trainname"

runWithCfg :: [FilePath] -> IO ()
runWithCfg [fname] = do 
  cfg                         <- readConfig fname   
  t0                          <- getTime Realtime
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
        
runGP :: Config -> IO (Individual, Maybe Handle, Individual -> Maybe [Double], Population Individual)
runGP cfg@(Conf mutCfg _ algCfg cnstCfg) = do
  g <- case (_seed . _algorithmCfg) cfg of
         Nothing -> getStdGen
         Just x  -> return $ mkStdGen x
  (train, val, alldata, test, domains, image, nvars) <- processData cfg
  let
      budget       = max 5 $ min 15 $ size (snd train) `div` 10
      mutCfg'      = mutCfg{ _yfuns = filterImage image (_yfuns mutCfg), _vars = [0 .. nvars-1], _budget=budget }
      task         = _task algCfg
      fits         = _fitness algCfg
      measures     = _measures algCfg
      penalty      = _penaltyType cnstCfg
      cnstr        = case _evaluator cnstCfg of 
                       Nothing -> const 0.0 
                       Just e  -> getViolationFun e (_shapes cnstCfg) (_domains cnstCfg)
      fitnessTrain = evalTrain task False fits measures cnstr penalty domains (fst train) (snd train) (fst val) (snd val)
      fitnessAll   = evalTrain task True fits measures cnstr penalty domains (fst alldata) (snd alldata) (fst alldata) (snd alldata)
      fitnessTest  = evalTest task measures (fst test) (snd test)
      
      -- dissimilarity function
      fromJust (Just x) = x

      toTheLeft x | x < 0 = 1.3*x 
                  | otherwise = 0.7*x 
      toTheRight x | x < 0 = 0.7*x 
                   | otherwise = 1.3*x 
      getNewLo (Just x) = (toTheLeft x, x)
      getNewHi (Just x) = (x, toTheRight x) 

      genListPoints n (lo, hi) = [lo, lo + (hi-lo)/(fromIntegral n ** (1 / fromIntegral nvars)) .. hi]
      band n = [genListPoints n (getNewLo $ inf xy) <> genListPoints n (getNewHi $ sup xy) | xy <- V.toList domains]
      distSample n = V.fromList $ map LA.fromList $ transpose $ map (1:) $ take n $ sequence (band n)
      distFun pts x y = (1 -) $ min 1 $ mse / max var1 var2
        where 
            p1 = LA.toList $ evalToTarget task pts x 
            p2 = LA.toList $ evalToTarget task pts y
            diff a b | (isNaN a || isInfinite a) && (isNaN b || isInfinite b) = 0
                     | (isNaN a || isInfinite a) || (isNaN b || isInfinite b) = 0 -- 1 
                     | otherwise = (a - b)*(a - b) -- let d = a - b in 1 - exp (-0.5*(d/h)^2)
            diffs = zipWith diff p1 p2
            mse = sum diffs / fromIntegral (length diffs)
            var1 = var p1 
            var2 = var p2 
            var :: [Double] -> Double
            var xs = sum' / fromIntegral (length xs)
              where
                mu   = sum xs / fromIntegral (length xs) 
                sum' = foldl (\s x -> s + (x-mu)^(2 :: Int)) 0 xs

            h = 0.01
      myDist = distFun (distSample 100)
      
      myCX OnePoint  = onepoint
      myCX UniformCX = uniformCx
      myMut GroupMutation = multiMut mutCfg'
      myCreate = createIndividual myDist <$> randomTIR mutCfg'
      myFitness = pure . fitnessTrain

            

      interpret :: Interpreter Individual
      interpret = Funs myCX myMut myCreate myFitness
      
      gp           = Reproduce Generational
                       [Cross OnePoint 2 (_pc algCfg) (Tournament 2) 
                            :> Mutate GroupMutation (_pm algCfg) :> Done]
      fi           = Reproduce Merge
                       [ With Feasible 
                             :> Cross OnePoint 2 (_pc algCfg) (Tournament 2) 
                             :> Mutate GroupMutation (_pm algCfg) :> Done
                       , With Infeasible 
                             :> Cross OnePoint 2 (_pc algCfg) (Tournament 2) 
                             :> Mutate GroupMutation (_pm algCfg) :> Done
                       ]
      moo          = Reproduce NonDominated
                       [Done
                         , Cross OnePoint 2 (_pc algCfg) (CrowdingTournament 2)
                               :> Mutate GroupMutation (_pm algCfg) :> Done]

      fs          = Reproduce (Probabilistic (FitShare 0.6 2))
                      [Done , Cross OnePoint 2 (_pc algCfg) (Tournament 2)
                           :> Mutate GroupMutation (_pm algCfg) :> Done]

      alg          = case _algorithm algCfg of
                       GPTIR -> gp
                       SCTIR -> fi
                       MOO   -> moo
                       FS    -> fs 
  (logger, mh)  <- makeLogger cfg fitnessTest
  (_, champion, front) <- runEvolution (_gens algCfg) (_nPop algCfg) logger alg g interpret 
  
  let -- bic model  = let n = size (snd train) in n * ln(mse) + n(1 + ln(2pi)) + ln(n)k
      getMyTree x = showPython $ getTree task (head . _weights $ x) (_chromo x)
      printMyTree = putStrLn . getMyTree
      champion'  = V.minimumBy (compare `on` (head . _getFitness)) front 
      getThr x   = if x < 0 then 0.95*x else 1.05*x 
      thr        = getThr . head . _getFitness $ champion'
      champion'' = if _algorithm  algCfg == MOO
                      then V.minimumBy (compare `on` ((!!1) . _getFitness)) $ V.filter ((<=thr) . head . _getFitness) front
                      else champion

  mapM_ (\i -> printMyTree i >> print (head $ _getFitness i) >> putStrLn "") front
  print (head $ _fit champion'')
  return (fitnessAll champion'', mh, fitnessTest, front)

getTree :: Task -> LA.Vector Double -> TIR -> SRTree Int Double
getTree task w tir = 
  let bias   = V.head $ VS.convert w
      consts = V.tail $ VS.convert w
      sigm z = 1 / (1+exp(-z))
  in  case task of 
        Classification _ -> sigm $ assembleTree bias $ replaceConsts tir consts
        ClassMult      _ -> sigm $ assembleTree bias $ replaceConsts tir consts
        _                -> assembleTree bias $ replaceConsts tir consts

main :: IO ()
main = do
  args <- getArgs  
  case args of
    ("regress":opts)    -> parseCLI Regression opts
    ("regressNL":opts)  -> parseCLI (RegressionNL 0) opts
    ("class":opts)      -> parseCLI (Classification 0) opts
    ("multiclass":opts) -> parseCLI (ClassMult 0) opts
    ("config":opts)     -> runWithCfg opts
    _                   -> putStrLn "Usage: ./tir {regress/regressNL/class/multiclass/config} args"
