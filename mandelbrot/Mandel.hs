{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 -funbox-strict-fields -fvia-C -optc-O2 -fexcess-precision #-}
module Mandel where

import qualified Data.ByteString as B
import Control.Parallel.Strategies
import Foreign
import System.IO

mandelWrite w h = hPutStrLn h ("P4\n"++show w++" "++show w) >> 
                  B.hPutStr h (mandel w)

mandel w = let m = 2 / fromIntegral w
               cs = [fromIntegral y * m - 1 | y <- [0..w-1]]
           in B.concat . parBuffer 64 rwhnf . map (fst . unfolded w m) $ cs

unfolded w m ci = B.unfoldrN (w `div` 8) (next_x w m ci) 0

next_x !w !iw !ci !x
    | x >= w    = Nothing
    | otherwise = Just (loop_x w iw ci x 8 0, x+8)

loop_x !w !iw !ci !x !n !b
    | x < w && n == 0 = b
    | n /= 0          = loop_x w iw ci (x+1) (n-1) (b+b+v)
    | otherwise       = b * 2 ^ n
  where
    v = fractal 0 0 (fromIntegral x * iw - 1.5) ci 50

fractal :: Double -> Double -> Double -> Double -> Int -> Word8
fractal !r !i !cr !ci !k
    | r2 + i2 > 4 = 0
    | k == 0      = 1
    | otherwise   = fractal (r2-i2+cr) ((r+r)*i+ci) cr ci (k-1)
  where
    (!r2,!i2) = (r*r,i*i)
