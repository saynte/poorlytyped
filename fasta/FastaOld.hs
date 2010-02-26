{-# LANGUAGE BangPatterns #-}

--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- A lazy bytestring solution.
-- Unnecessary strictness annotations removed by Sterling Clover 2/08
--
-- Add:
-- -optc-mfpmath=sse -optc-msse2
--
module Main (main) where

import System
import Data.Word
import Control.Arrow

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C (pack,unfoldr)
import qualified Data.ByteString as S
import Data.ByteString.Internal
import Data.ByteString.Unsafe

main = do
    n <- getArgs >>= readIO . head
    writeFasta  "ONE"   "Homo sapiens alu"       (n*2) (L.cycle alu)
    g <- unfold "TWO"   "IUB ambiguity codes"    (n*3) (look iubs) 42
    unfold      "THREE" "Homo sapiens frequency" (n*5) (look homs) g

------------------------------------------------------------------------
--
-- lazily unfold the randomised dna sequences
--

unfold l t n f g = putStrLn (">" ++ l ++ " " ++ t) >> unroll f g n

unroll :: (Int -> (Word8, Int)) -> Int -> Int -> IO Int
unroll f = loop
    where
        loop r 0   = return r
        loop !r i = case S.unfoldrN m (Just . f) r of
                        (!s, Just r') -> do
                            S.putStrLn s
                            loop r' (i-m)
          where m = min i 60

look ds k = (choose ds d, j)
  where (d,j) = rand k

data PPair = PPair !Word8 !Float

choose :: [(Word8,Float)] -> Float -> Word8
choose [(b,_)]       _ = b
choose ((b,f):xs) p = if p < f then b else choose xs (p-f)

------------------------------------------------------------------------
--
-- only demand as much of the infinite sequence as we require

writeFasta label title n s = do
     putStrLn $ ">" ++ label ++ " " ++ title
     let (t:ts) = L.toChunks s
     go ts t n
  where
     go ss s n
        | l60 && n60 = S.putStrLn l               >> go ss        r (n-60)
        |        n60 = S.putStr s >> S.putStrLn a >> go (tail ss) b (n-60)
        | n <= ln    = S.putStrLn (S.take n s)
        | otherwise  = S.putStr s >> S.putStrLn (S.take (n-ln) (head ss))
        where
            ln   = S.length s
            l60  = ln >= 60
            n60  = n  >= 60
            (l,r) = S.splitAt 60 s
            (a,b) = S.splitAt (60-ln) (head ss)

------------------------------------------------------------------------
im, ia, ic :: Int
im  = 139968
ia  = 3877
ic  = 29573

rand :: Int -> (Float, Int)
rand seed = (newran,newseed)
    where
        newseed = (seed * ia + ic) `rem` im
        newran  =  1.0 * fromIntegral newseed / imd
        imd      = fromIntegral im

------------------------------------------------------------------------

alu = C.pack
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        \GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        \CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        \ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        \GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        \AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
        \AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iubs, homs :: [(Word8, Float)]
iubs = map (c2w *** id)
        [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homs = map (c2w *** id)
        [('a',0.3029549426680),('c',0.1979883004921)
        ,('g',0.1975473066391),('t',0.3015094502008)]