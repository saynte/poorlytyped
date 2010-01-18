{-# LANGUAGE BangPatterns #-}
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.Vector
import System
import System.IO

infixl 7 .*, *|

(.*) :: Vector3 -> Vector3 -> Double
(.*) = vdot

(*|) :: Double -> Vector3 -> Vector3
(*|) = (*<>)

len = vmag

vect :: Double -> Double -> Double -> Vector3
vect x y z = Vector3 x y z

infinity, delta :: Double
infinity = 1/0
delta = sqrt e where e = encodeFloat (floatRadix e) (-floatDigits e)

unitise :: Vector3 -> Vector3
unitise r = 1 / len r *| r

data Scene = S !Vector3 !Double [Scene]
data Hit = H { lam :: !Double, nv ::  Vector3}

ray_sphere :: Vector3 -> Vector3 -> Double -> Double
ray_sphere dir v radius =
    let  disc = v .* v - radius * radius
         b = v .* dir
         b2 = b*b
    in if disc < 0 || b2 < disc
       then infinity
       else let disk = sqrt (b2 - disc)
                t1 = b - disk
            in if t1 > 0 then t1 else b + disk

ray_sphere' :: Vector3 -> Vector3 -> Double -> Bool
ray_sphere' orig center radius =
    let v = center - orig
        b = v .* neg_light
        b2 = b * b
        rest = v .* v - radius * radius
    in  b2 >= rest && (b > 0 || rest < 0)

intersect dir first (S center radius scene) =
    let l' = ray_sphere dir center radius
    in  if l' >= lam first
        then first
        else case scene of
               [] -> H l' (unitise (l' *| dir - center))
               scenes -> foldl' (intersect dir) first scenes

intersect' orig (S center radius scenes) =
    ray_sphere' orig center radius && 
                    (null scenes || any (intersect' orig) scenes)

ray_trace dir scene =
    case intersect dir (H infinity 0) scene of
      H 0 _ -> infinity
      H lambda normal ->
          let g = normal .* light
          in  if g >= 0 then 0
              else let p = lambda *| dir + delta *| normal
                   in  if intersect' p scene then 0 else - g

neg_light = -light

bound (S c r s) (S c' r' []) = S c (max r (len (c - c') + r')) s
bound b (S  _ _ l) = foldl' bound b l

create 1 c r = S c r []
create level c r =
    let a = 3 * r / sqrt 12
        aux x' z' = create (level - 1 :: Int) (c + vect x' a z') (0.5 * r)
        l = [S c r [], aux (-a) (-a), aux a (-a), aux (-a) a, aux a a]
    in  foldl' bound (S (c + vect 0 r 0) 0 l) l

ss = 4

light = unitise (vect (-1) (-3) 2)

pixel_vals n scene y x =
    [ let 
        f a da = a - n / 2 + da / ss
        d = unitise (vect (f x dx) (f y dy) n)
   in ray_trace d scene | dx <- [0..ss-1], dy <- [0..ss-1] ]

main = do 
    [level,ni] <- fmap (map read) getArgs
    let n       = fromIntegral ni
        scene   = create level (vect 0 (-1) 4) 1  
        scale x = 0.5 + 255 * x / (ss*ss)
        f y x   = toEnum . truncate . scale . sum $ pixel_vals n scene y x
        picture' = [ f y x | y <- [n-1,n-2..0], x <- [0..n-1]]
        picture = parBuffer ni rwhnf picture'
    hSetEncoding stdout latin1
    putStr $ "P5\n" ++ show ni ++ " " ++ show ni ++ "\n255\n" ++ picture