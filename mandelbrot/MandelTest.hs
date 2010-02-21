module Main where

import qualified Mandel as New
import qualified MandelOrig as Orig

import Criterion.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO


main = 
  let act1 = withFile "mandel.pgm" WriteMode . New.mandelWrite 
      act2 = withFile "mandelOrig.pgm" WriteMode . Orig.mandelWrite
  in 
    defaultMain 
       [bgroup "mandel" [ bench "mandel" (act1 1000)
                        , bench "mandelOrig" (act2 1000)
                        ]
       ]
