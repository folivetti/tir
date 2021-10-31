module MachineLearning.TIR.Mutation where

import MachineLearning.TIR
import MachineLearning.Utils.Config
import Control.Evolution
import Control.Monad.State.Strict
import System.Random
import Data.List (sortOn, groupBy, (\\))

toss :: Rnd Bool 
toss = state random
{-# INLINE toss #-}

randomChoice :: Rnd a -> Rnd a -> Rnd a
randomChoice f g = do
  coin <- toss
  if coin
     then f
     else g
{-# INLINE randomChoice #-}

trd :: Pi -> [(Int, Int)]
trd (_,_,x) = x
{-# INLINE trd #-}

countVars :: Sigma -> Int
countVars = foldr (\(_,_,x) acc -> length x + acc) 0
{-# INLINE countVars #-}

applyMut :: MutationCfg -> Individual -> (MutationCfg -> TIR -> Rnd TIR) -> Rnd Individual
applyMut params x mut = do
  t <- mut params (_chromo x)
  pure x{ _chromo=t, _fit=[] }

multiMut :: MutationCfg -> Individual -> Rnd Individual
multiMut params x = do
  let (TIR _ p q) = _chromo x
  mut <- if null p && null q
          then randomFrom [insertNode]
          else if countVars p + countVars q >= _budget params
                 then randomFrom [removeNode, changeVar, changeExponent, changeFun]
                 else randomFrom [insertNode, removeNode, changeVar, changeExponent, changeFun]
  applyMut params x mut

insertNode :: MutationCfg -> TIR -> Rnd TIR
insertNode params (TIR g p q) 
  | null p && null q = insertTerm
  | otherwise        = randomChoice insertVar insertTerm
  where
    insertVar = do
      let np = countVars p
          nq = countVars q
      ix <- randomRng (0, np+nq-1)
      k  <- randomRngNZ $ _kRange params 
      if ix < np
         then do (var, _) <- randomVar params
                 case var of
                   Nothing -> pure (TIR g p q)
                   Just v  -> pure (TIR g (insertInto ix p (v,k)) q)
         else do (var, _) <- randomVar params
                 case var of
                   Nothing -> pure (TIR g p q)
                   Just v  -> pure (TIR g p (insertInto (ix - np) q (v,k)))
    insertTerm = do
      let np = length p
          nq = length q
      ix <- randomRng (0, np+nq-1)
      if ix < np
         then do mp <- randomPi params
                 case mp of
                   Nothing -> pure (TIR g p q)
                   Just p' -> pure (TIR g (p':p) q)
         else do mq <- randomPi params
                 case mq of
                   Nothing -> pure (TIR g p q)
                   Just q' -> pure (TIR g p (q':q))
    
    insertInto ix  []    _ = error "Incorrect index in insertVar"
    insertInto 0  (x:xs) y = appendVar x y : xs
    insertInto ix (x:xs) y = let nvars = length (trd x)
                             in  if nvars >= ix
                                  then appendVar x y : xs
                                  else x : insertInto (ix - nvars) xs y
    appendVar (x, y, z) z' = if z' `elem` z then (x, y, z) else (x, y, z':z)

removeNode :: MutationCfg -> TIR -> Rnd TIR
removeNode params (TIR g p q) = randomChoice removeVar removeTerm
  where
    removeVar = do
      let np = countVars p
          nq = countVars q
      ix <- randomRng (0, np+nq-1)
      if ix < np
         then if np <= 1 then pure (TIR g p q) else pure (TIR g (removeVarAt ix p) q) 
         else pure (TIR g p (removeVarAt (ix-np) q))
    removeTerm = do
      let np = length p
          nq = length q
      ix <- randomRng (0, np+nq-1)
      if ix < np
         then if np <= 1 then pure (TIR g p q) else pure (TIR g (removeAt ix p) q)
         else pure (TIR g p (removeAt (ix-np) q))

    removeAt ix xs         = take ix xs ++ drop (ix+1) xs
    removeVarAt ix []            = []
    removeVarAt ix ((a,b,c):xs)  = let nvars = length c
                                   in if ix < nvars
                                       then if nvars == 1
                                             then xs
                                             else (a, b, removeAt ix c) : xs
                                       else (a,b,c) : removeVarAt (ix - nvars) xs

changeVar :: MutationCfg -> TIR -> Rnd TIR
changeVar params (TIR g p q) = do
  let np = countVars p
      nq = countVars q
  ix <- randomRng (0, np+nq-1)
  if ix < np
    then do p' <- changeVarAt ix p
            pure (TIR g p' q)
    else do q' <- changeVarAt (ix-np) q
            pure (TIR g p q')
  where
    
    changeVarAt ix [] = error $ "changeVarAt sampled something wrong" <> show p <> show q <> show ix
    changeVarAt ix ((w,x,ys):xs)
      | ix < length ys = do ys' <- changeElemAt ix ys
                            pure $ (w,x,ys'):xs
      | otherwise      = ((w,x,ys) :) <$> changeVarAt (ix - length ys) xs
    changeElemAt ix ys = do let y  = ys !! ix
                                vs = _vars params \\ map fst ys
                            if null vs
                              then pure ys
                              else do v' <- randomFrom vs
                                      pure $ take ix ys ++ ((v', snd y) : drop (ix+1) ys)

changeExponent :: MutationCfg -> TIR -> Rnd TIR
changeExponent params (TIR g p q) = do
  let np = countVars p
      nq = countVars q
  ix <- randomRng (0, np+nq-1)
  if ix < np
    then do p' <- changeVarAt ix p
            pure (TIR g p' q)
    else do q' <- changeVarAt (ix-np) q
            pure (TIR g p q')
  where
    
    changeVarAt ix [] = error "changeExp sampled something wrong"
    changeVarAt ix ((w,x,ys):xs)
      | ix < length ys = do ys' <- changeElemAt ix ys
                            pure $ (w,x,ys'):xs
      | otherwise      = ((w,x,ys) :) <$> changeVarAt (ix - length ys) xs
    changeElemAt ix ys = do let y  = ys !! ix
                            k <- randomRngNZ $ _kRange params
                            pure $ take ix ys ++ ((fst y, k) : drop (ix+1) ys)

changeFun :: MutationCfg -> TIR -> Rnd TIR
changeFun params (TIR g p q) = do
  let np = length p
      nq = length q
  ix <- randomRng (0, np+nq)
  g' <- randomFrom (_yfuns params)
  f' <- randomFrom (_funs params)
  if ix == 0
    then pure (TIR g' p q)
    else if ix <= np
           then pure (TIR g (changeFunAt (ix-1) p f') q)
           else pure (TIR g p (changeFunAt (ix-1-np) q f'))
  where
    changeFunAt _  []            h' = error "Wrong indice sampled from changeFun"
    changeFunAt 0  ((w,h,ys):xs) h' = (w,h',ys) : xs
    changeFunAt ix ((w,h,ys):xs) h' = (w,h,ys)  : changeFunAt (ix-1) xs h' 

replaceSubTree :: MutationCfg -> TIR -> Rnd TIR
replaceSubTree = undefined
