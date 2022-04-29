{-|
Module      : MachineLearning.Utils.Data
Description : TIR expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Dataset handling functions.

-}
module MachineLearning.Utils.Data (processData) where

import Data.List                             (transpose)
import Data.List.Split                       (splitOn)
import Data.Vector                           (Vector, fromList)
import Numeric.ModalInterval            (Kaucher, (<.<))
import Numeric.LinearAlgebra ((??))
import qualified Numeric.LinearAlgebra as LA
import Numeric.Morpheus.MatrixReduce         (columnPredicate)

-- import Constraints.Shape
import MachineLearning.Utils.Config
import MachineLearning.TIR

-- | A tuple of features columns and target
type DataSplit = (Dataset Double, Column Double)

-- | Reads and loads the data. It returns the training X, y, test X, y, vector of domains and image of the training data,
-- number of variables
processData :: Config -> IO (DataSplit, DataSplit, DataSplit, DataSplit, Vector (Kaucher Double), Kaucher Double, Int)
processData cfg =
  do (trainX, trainY) <- readAndParse (getTrainName cfg)
     (testX , testY ) <- readAndParse (getTestName  cfg)
     let nVars    = LA.cols trainX - 1
         domains  = fromList $ estimateDomains (getDomains cfg) trainX
         image    = estimateImage (getImage cfg) trainY 
         xss_all  = toVecOfColumns trainX
         xss_test = toVecOfColumns testX
         (xss_train, y_train, xss_val, y_val) = splitValidation 0.9 trainX trainY
     return ((xss_train, y_train), (xss_val, y_val), (xss_all, trainY), (xss_test, testY), domains, image, nVars)

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> (LA.Matrix Double, Column Double)
parseFile css = splitToXY . LA.fromLists $ map (map read) dat
  where
    dat = map (splitOn ",") $ lines css
    
-- | read and parse the csv file
readAndParse :: FilePath -> IO (LA.Matrix Double, LA.Vector Double)
readAndParse f = do (xss, ys) <- parseFile <$> readFile f
                    return (1.0 LA.||| xss, ys)

toVecOfColumns :: LA.Matrix Double -> Dataset Double
toVecOfColumns = fromList . LA.toColumns

takeNRows, dropNRows :: Int -> LA.Matrix Double -> LA.Matrix Double
takeNRows n xss = xss LA.?? (LA.Take n, LA.All)
dropNRows n xss = xss LA.?? (LA.Drop n, LA.All)

-- | split the training data into training and validation
splitValidation :: Double -> LA.Matrix Double -> LA.Vector Double 
                -> (Dataset Double, LA.Vector Double, Dataset Double, LA.Vector Double)
splitValidation ratio xss ys
  | nRows <= 50 = (toVecOfColumns xss, ys, toVecOfColumns xss, ys)
  | otherwise    = (xss_train, y_train, xss_val, y_val)
  where
    nRows      = LA.rows xss
    nRowsTrain = round (fromIntegral nRows * ratio)
    nRowsVal   = nRows - nRowsTrain
    xss_train  = toVecOfColumns $ takeNRows nRowsTrain xss
    xss_val    = toVecOfColumns $ dropNRows nRowsVal xss
    y_train    = LA.subVector 0 nRowsTrain ys
    y_val      = LA.subVector nRowsVal nRowsTrain ys    

-- | estimates the target image of the function    
estimateImage :: Maybe (Double, Double) -> LA.Vector Double -> Kaucher Double
estimateImage image ys = 
  case image of 
       Nothing       -> minY <.< maxY
       Just (lo, hi) -> lo <.< hi
  where
    minY = minimum $ LA.toList ys
    maxY = maximum $ LA.toList ys

type Domains = [(Double, Double)]

-- | estimates the target domain of the function    
estimateDomains :: Domains -> LA.Matrix Double -> [Kaucher Double]
estimateDomains domains xss =
  case domains of
    [] -> zipWith (<.<) minX maxX
    ds -> map (uncurry (<.<)) ds
  where
    minX = Prelude.tail $ LA.toList $ columnPredicate min xss
    maxX = Prelude.tail $ LA.toList $ columnPredicate max xss    
    
splitToXY :: LA.Matrix Double -> (LA.Matrix Double, LA.Vector Double)
splitToXY mtx = (xss, ys)
  where 
    xss = mtx ?? (LA.All, LA.DropLast 1)
    ys  = LA.flatten $ mtx ?? (LA.All, LA.TakeLast 1)
