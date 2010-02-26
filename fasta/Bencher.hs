module Main where

import Control.Arrow

import qualified Data.ByteString as S
import Data.ByteString.Internal

import Data.List
import Data.Word

import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Primitive as P

import Criterion.Main

n = 1000000
seed = 42

main = 
    defaultMain 
    [ bgroup "chooser" 
      [ bench "list" $ whnf runnerNorm iubs
--      , bench "expand" $ whnf (runner chooseExp) iubs
--      , bench "PP" $ whnf (runner choosePPair) (mkPPairs iubs)
      , bench "cache" $ whnf runnerCache (mkCacher iubs)
--      , bench "expand PP" $ whnf (runner chooseExpPPair) (mkPPairs iubs)
--      , bench "vec PP" $ whnf (runner chooseVec) (mkPPairV iubs)
--      , bench "vec fold PP" $ whnf (runner chooseVecFold) (mkPPairV iubs)
--      , bench "vec find PP" $ whnf (runner chooseVecFind) (mkPPairV $ cdfize iubs)
--      , bench "vec findU PP" $ whnf (runner chooseVecFindIndex) (mkPairV iubs)

      ]
    ]

lookNorm ds k = (choose ds d, j)
    where R d j = rand k

{-# INLINE lookCache #-}
lookCache cr k = (chooseCache cr d, j)
    where R d j = rand k

runnerNorm ds = S.unfoldrN n (Just . lookNorm ds) seed
runnerCache ds = S.unfoldrN n (Just . lookCache ds) seed

look c ds k = (c ds d, j)
  where R d j = rand k

choose :: [(Word8,Float)] -> Float -> Word8
choose [(b,_)]    _ = b
choose ((b,f):xs) p = if p < f then b else choose xs (p-f)

chooseExp :: [(Word8,Float)] -> Float -> Word8
chooseExp [(b,_)]    _ = b
chooseExp ((b1,f1):(b2,f2):(b3,f3):(b4,f4):xs) p = 
    let p2 = p - f1
        p3 = p2 - f2
        p4 = p3 - f3
        r 
            | p < f1 = b1
            | p2 < f2 = b1
            | p3 < f3 = b1
            | p4 < f4 = b1
            | otherwise = choose xs (p4 - f4)
    in r
chooseExp ds p = choose ds p

mkPPairs = map (uncurry PPair)
mkPPairV = V.fromList . map (uncurry PPair)
mkPairV :: [(Word8, Float)] -> (U.Vector Word8, U.Vector Float)
mkPairV ds = (U.fromList (map fst ds') , U.fromList (map snd ds'))
    where ds' = cdfize ds

chooseExpPPair :: [PPair] -> Float -> Word8
chooseExpPPair [PPair b _]    _ = b
chooseExpPPair ((PPair b1 f1):(PPair b2 f2):(PPair b3 f3):(PPair b4 f4):xs) p = 
    let p2 = p - f1
        p3 = p2 - f2
        p4 = p3 - f3
        r 
            | p < f1 = b1
            | p2 < f2 = b1
            | p3 < f3 = b1
            | p4 < f4 = b1
            | otherwise = choosePPair xs (p4 - f4)
    in r

choosePPair [PPair b _]    _ = b
choosePPair ((PPair b f):xs) p = if p < f then b else choosePPair xs (p - f)

chooseVec = chooseVec' 0

chooseVec' :: Int -> Vector PPair -> Float -> Word8
chooseVec' i v p 
    | V.length v == 1 = sym $ V.head v
    | otherwise = let PPair b f = v `V.unsafeIndex` i
                  in if p < f 
                     then b
                     else chooseVec' (i+1) v (p - f)

chooseVecFold v p = 
    case fst (V.foldr' (flip chooseFolder) (Nothing, p) v) of
      Just pp -> sym pp
      Nothing -> sym (V.unsafeLast v)


chooseFolder :: (Maybe PPair, Float) -> PPair -> (Maybe PPair, Float)
chooseFolder (Nothing, p) pp@(PPair s f) 
    | p < f     = (Just pp, p)
    | otherwise = (Nothing, p - f)
chooseFolder x _ = x

--chooseVecFind v p = sym (maybe (U.unsafeLast v) id (U.find ( (p <) . pr) v))
--chooseVecFindIndex :: (U.Vector Word8, U.Vector Float) -> Float -> Word8
--chooseVecFindIndex (v,stripV) p = 
--    v `U.unsafeIndex` (chooseVecFindIndex' stripV p)

--chooseVecFindIndex' v p = maybe (P.length v - 1) id (P.findIndex (p < ) v)

cdfize :: [(Word8,Float)] -> [(Word8,Float)]
cdfize = snd . mapAccumL go 0
    where go c (sym, prob) = (c + prob, (sym, c+prob))

data Cacher = Cacher !PPair !PPair !PPair !PPair [(Word8, Float)]
data PPair = PPair {sym :: !Word8, pr :: !Float}

mkCacher ds = Cacher p1 p2 p3 p4 (drop 4 ds)
    where
      [p1,p2,p3,p4] = map (uncurry PPair) (take 4 ds)

chooseCache :: Cacher -> Float -> Word8
chooseCache (Cacher 
         (PPair s1 c1)
         (PPair s2 c2)
         (PPair s3 c3)
         (PPair s4 c4)
         ds) p 
    | p < c1 = s1
    | p < c2 = s2
    | p < c3 = s3
    | p < c4 = s4
    | otherwise = case ds of 
                    [] -> s4
                    _  -> chooseCdf ds p


chooseCdf :: [(Word8,Float)] -> Float -> Word8
chooseCdf [(b,_)]       _ = b
chooseCdf ((b,f):xs) p = if p < f then b else chooseCdf xs p

iubs, homs :: [(Word8, Float)]
iubs = map (c2w *** id)
        [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]
homs = map (c2w *** id)
        [('a',0.3029549426680),('c',0.1979883004921)
        ,('g',0.1975473066391),('t',0.3015094502008)]

im, ia, ic :: Int
im  = 139968
ia  = 3877
ic  = 29573

data R = R !Float !Int

rand :: Int -> R
rand seed = R newran newseed
    where
        newseed = (seed * ia + ic) `rem` im
        newran  =  1.0 * fromIntegral newseed / imd
        imd      = fromIntegral im