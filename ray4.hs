import Control.Parallel.Strategies
import System
import System.IO
infinity = 1/0
delta = sqrt e where e = encodeFloat (floatRadix e) (-floatDigits e)
infixl 7 .*, *|
data Vector = V !Double !Double !Double deriving (Show, Eq)
s *| V x y z = V (s * x) (s * y) (s * z)
instance Num Vector where
    V x y z + V x' y' z' = V (x + x') (y + y') (z + z')
    V x y z - V x' y' z' = V (x - x') (y - y') (z - z')
    fromInteger i = V x x x where x = fromInteger i
V x y z .* V x' y' z' = x * x' + y * y' + z * z'
vlength r = sqrt (r .* r)
unitise r = 1 / vlength r *| r
data Scene = S !Vector !Double [Scene]
ray_sphere dir v radius =
    let b = v .* dir
	disc = b * b - v .* v + radius * radius
    in  if disc < 0 then infinity
	else
	    let sdisc = sqrt disc
		t2 =  b + sdisc
	    in  if t2 < 0 then infinity
		else
		    let t1 = b - sdisc
		    in  if t1 > 0 then t1 else t2
ray_sphere' orig dir center radius =
    let v = center - orig
	b = v .* dir
	disc = b * b - v .* v + radius * radius
    in  disc >= 0 && b + sqrt disc >= 0
intersect dir first@(l, _) (S center radius scene) =
    let l' = ray_sphere dir center radius
    in  if l' >= l then first
	else case scene of
        	[] -> (l', unitise (l' *| dir - center))
		scenes -> foldl (intersect dir) first scenes
intersect' orig dir (S center radius scenes) =
    ray_sphere' orig dir center radius && (null scenes || any (intersect' orig dir) scenes)
ray_trace light dir scene =
    case intersect dir (infinity, 0) scene of
	(0, _) -> infinity
	(lambda, normal) ->
	    let g = normal .* light
	    in  if g >= 0 then 0
		else let p = lambda *| dir + delta *| normal
		     in  if intersect' p (-light) scene then 0 else - g
bound (S c r s) (S c' r' []) = S c (max r (vlength (c - c') + r')) s
bound b (S  _ _ l) = foldl bound b l
create 1 c r = S c r []
create level c r =
    let a = 3 * r / sqrt 12
	aux x' z' = create (level - 1 :: Int) (c + V x' a z') (0.5 * r)
	l = [S c r [], aux (-a) (-a), aux a (-a), aux (-a) a, aux a a]
    in  foldl bound (S (c + V 0 r 0) 0 l) l
ss = 4
light = unitise (V (-1) (-3) 2)
pixel_vals n scene y x =
  [ let f a da = a - n / 2 + da / ss; d = unitise (V (f x dx) (f y dy) n)
    in  ray_trace light d scene | dx <- [0..ss-1], dy <- [0..ss-1] ]
main = do 
    [level,ni] <- fmap (map read) getArgs
    let n = fromIntegral ni
	scene = create level (V 0 (-1) 4) 1  
	scale x = 0.5 + 255 * x / (ss*ss)
	picture = parBuffer ni rwhnf [ toEnum $ truncate $ scale $ sum $ pixel_vals n scene y x | y <- [n-1,n-2..0], x <- [0..n-1]]
    hSetEncoding stdout latin1
    putStr $ "P5\n" ++ show ni ++ " " ++ show ni ++ "\n255\n" ++ picture
