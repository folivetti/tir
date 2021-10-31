module MachineLearning.TIR.Selection where

import MachineLearning.TIR
import Control.Evolution
import Control.Monad.State.Strict
import System.Random
import qualified Data.Vector as V

tournament :: Int -> Population Individual -> Rnd Individual
tournament n pop = do
  (ix:ixs) <- replicateM n (randomRng (0, V.length pop - 1))
  pure $ foldr (\i p -> min (pop V.! i) p) (pop V.! ix) ixs 

generational :: Population Individual -> Population Individual -> Rnd (Population Individual)
generational _ = pure
