module Main where
import Control.Parallel.Strategies
import System
import System.IO
infinity = 1/0
sq_epsilon_float = sqrt e where e = encodeFloat (floatRadix e) (-floatDigits e)

infixl 7 .*, *|
data Vector = V !Double !Double !Double deriving (Show, Eq)
s *| V x y z = V (s * x) (s * y) (s * z)
instance Num Vector where
    V x y z + V x' y' z' = V (x + x') (y + y') (z + z')
    V x y z - V x' y' z' = V (x - x') (y - y') (z - z')
    fromInteger i = V x x x where x = fromInteger i
V x y z .* V x' y' z' = x * x' + y * y' + z * z'
unitise r = 1 / sqrt (r .* r) *| r

data Scene = S !Vector !Double [Scene]

intersect d hit@(l, _) (S c r s) =
    let b = c .* d
        b2 = b*b
        rest = c .* c - r * r
        l' = if b2 < rest || (b <= 0 && rest >= 0)
             then infinity
             else let sqdisc = sqrt (b2 - rest)
                      t1 = b - sqdisc
                  in if t1 > 0 then t1 else b + sqdisc               
    in  if l' >= l 
        then hit 
        else case s of
               [] -> (l', unitise (l' *| d - c))
               ss -> foldl (intersect d) hit ss

intersect' o d (S c r s) =
  let v = c - o
      b = v .* d
      b2 = b*b
      rest = v .* v - r * r
      notInf = b2 >= rest && (b > 0 || rest < 0)
  in  notInf && (null s || any (intersect' o d) s)

light = unitise (V 1 3 (-2)); ss = 4

create 1 c r = S c r []
create level c r =
    let a = 3 * r / sqrt 12
	aux x' z' = create (level - 1) (c + V x' a z') (0.5 * r)
    in  S c (3*r) [S c r [], aux (-a) (-a), aux a (-a), aux (-a) a, aux a a]

ray_trace dir scene =
  let (l, n) = intersect dir (infinity, 0) scene
      g = n .* light
  in  if g <= 0 then 0 else
      let p = l *| dir + sq_epsilon_float *| n
      in if intersect' p light scene then 0 else g

pixel_val n scene y x = sum
  [ let f a da = a - n / 2 + da / ss; d = unitise (V (f x dx) (f y dy) n)
    in  ray_trace d scene | dx <- [0..ss-1], dy <- [0..ss-1] ]

main = do 
    [level,ni] <- fmap (map read) getArgs
    let n = fromIntegral ni
	scene = create level (V 0 (-1) 4) 1  
	scale x = 0.5 + 255 * x / (ss*ss)
	picture = parBuffer ni rwhnf [ toEnum $ truncate $ scale $ pixel_val n scene y x | y <- [n-1,n-2..0], x <- [0..n-1]]
    hSetEncoding stdout latin1
    putStr $ "P5\n" ++ show ni ++ " " ++ show ni ++ "\n255\n" ++ picture
