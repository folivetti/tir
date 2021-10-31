module MachineLearning.TIR where

import Control.Evolution
import Control.Monad.State.Strict
import Control.DeepSeq (NFData, rnf)
import System.Random
import Data.List (delete)
import Data.List.Split
import Data.SRTree
import Data.SRTree.Print (showDefault)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable  as VS
import qualified Numeric.LinearAlgebra                     as LA
import MachineLearning.Utils.Config

data TIR = TIR { _funY :: Function
               , _p :: Sigma
               , _q :: Sigma
               } deriving Show

instance NFData TIR where
  rnf _ = ()
  
type Sigma = [Pi]
type Pi    = (Double, Function, [(Int, Int)])

randomRng :: (Int, Int) -> Rnd Int
randomRng rng = state $ randomR rng
{-# INLINE randomRng #-}

randomRngNZ :: (Int, Int) -> Rnd Int
randomRngNZ rng = do
  x <- randomRng rng
  if x == 0
    then randomRngNZ rng
    else pure x
{-# INLINE randomRngNZ #-}

randomFrom :: [a] -> Rnd a
randomFrom xs = do
  ix <- randomRng (0, length xs - 1)
  pure (xs !! ix)
{-# INLINE randomFrom #-}

randomVar :: MutationCfg -> Rnd (Maybe Int, MutationCfg)
randomVar params = do
  let vars = _vars params
      n    = length vars
  ix <- randomRng (0, n)
  if ix == n
     then pure (Nothing, params)
     else do let x = vars !! ix
             pure (Just x, params{ _vars=delete x vars })

randomVars :: MutationCfg -> Rnd [(Int, Int)]
randomVars params = do
  (v, params') <- randomVar params
  k            <- randomRngNZ $ _kRange params 
  case v of
    Nothing  -> pure []
    Just var -> do vs <- randomVars params'
                   pure $ (var, k) : vs

randomPi :: MutationCfg -> Rnd (Maybe Pi)
randomPi params = do
  pis <- randomVars params
  f   <- randomFrom $ _funs params
  if null pis
    then pure Nothing
    else pure $ Just (1.0, f, pis)

randomSigma :: MutationCfg -> Int -> Rnd (Sigma, Int)
randomSigma params budget | budget <= 0 = pure ([], budget)
randomSigma params budget = do
  n <- randomRng (0, budget)
  if n == budget
     then pure ([], budget)
     else do term             <- randomPi params
             (terms, budget') <- randomSigma params (budget - spentBudget term)
             case term of
               Nothing -> pure (terms, budget')
               Just t  -> pure (t:terms, budget')

  where 
    spentBudget Nothing           = 0
    spentBudget (Just (_, _, ps)) = 1 -- length ps

randomTIR :: MutationCfg -> Rnd TIR
randomTIR params = do
  yf           <- randomFrom $ _yfuns params
  (p, budget') <- randomSigma params $ _budget params
  (q, _)       <- randomSigma params budget'
  if null p
    then randomTIR params
    else pure (TIR yf p q)
  
type Column a   = LA.Vector a
type Dataset a  = Vector (Column a)
type Constraint = SRTree Int Double -> Double

data Individual = Individual { _chromo  :: TIR 
                             , _fit     :: [Double]
                             , _weights :: [LA.Vector Double]
                             , _constr  :: Double
                             , _len     :: Int
                             , _penalty :: Double
                             }

createIndividual :: TIR -> Individual
createIndividual tir = Individual tir [] [] 0.0 0 0.0

penalizedFit :: Individual -> Double
penalizedFit t = (head . _fit) t + _penalty t
{-# INLINE penalizedFit #-}

replaceConsts :: TIR -> V.Vector Double -> TIR
-- replaceConsts (TIR g p q) ws | V.length ws < length p + length q = error $ show p <> show q <> show ws
replaceConsts (TIR g p q) ws = TIR g p' q'
  where
    (p', ws1) = runState (traverse replaceWeight p) (V.toList ws)
    (q', ws2) = runState (traverse replaceWeight q) ws1
       
replaceWeight :: Pi -> State [Double] Pi
replaceWeight (w, g, h) = state $ \ws -> case ws of
                                           (wi:ws') -> ((wi, g, h), ws')
                                           []       -> error $ show h -- ((w, g, h), [])

instance Eq Individual where
  t1 == t2 = penalizedFit t1 == penalizedFit t2
instance Ord Individual where
  t1 <= t2 = penalizedFit t1 <= penalizedFit t2

instance NFData Individual where
  rnf _ = ()
 
instance Solution Individual where
  _getFitness = head . _fit 
  _isFeasible = (==0.0) . _constr
  
assembleTree :: Double -> TIR -> SRTree Int Double
assembleTree bias (TIR f p q) = Fun f ((Const bias + assemble p) / (1 + assemble q))
  where
    -- assemble :: Sigma ix val -> SRTree ix val
    assemble []      = 0
    assemble [p']    = mk p'
    assemble (p':ps) = mk p' + assemble ps

    -- mk :: Pi ix val -> SRTree ix val
    mk (v, g, ts) = Const v * Fun g (foldr (\(ix, k) acc -> acc * (Pow (Var ix) k)) 1 ts)

prettyPrintsolution :: Individual -> String
prettyPrintsolution sol | Prelude.null (_fit sol) = error "unevaluated solution"
prettyPrintsolution sol = concat [ "Expression:\n", (showDefault . assembleTree bias . _chromo) sol, "\n"
                                 , "Fitness: ", (show . head . _fit) sol, "\n"
                                 , "Constraints: ", (show . _constr) sol, "\n"
                                 , "Length: ", (show . _len) sol, "\n"
                                 , "Penalty: ", (show . _penalty) sol, "\n"
                                 ]

  where bias = V.head $ VS.convert $ head $ _weights sol
