import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace

test = pt2 <$> readFile "test24.txt"

main = pt2 <$> readFile "input24.txt"

pt2 = map fst . map parse . lines

parse :: String -> Line
parse line = let [x, y, z, _, v, u, w] = map read $ words $ filter (/= ',') line in ((x, y, z), (v, u, w))

mn = 200000000000000

mx = 400000000000000

type Line = ((Float, Float, Float), (Float, Float, Float))

-- solve :: [Line] -> [(Line, Line)]
solve lines =
  [ [(x, y)]
    | (i, a) <- zip [0 ..] lines,
      (j, b) <- zip [0 ..] lines,
      i < j,
      let (m1, c1) = line a,
      let (m2, c2) = line b,
      let (x, y) = intersectionPoint (m1, c1) (m2, c2),
      movingTowards (x, y) a,
      movingTowards (x, y) b,
      not $ isInfinite x,
      not $ isInfinite y,
      inTestArea (x, y)
  ]

inTestArea (x, y) = x >= mn && x <= mx && y >= mn && y <= mx

line ((x, y, z), (v, u, w)) = (m, c)
  where
    c = y - m * x
    m = u / v

intersectionPoint (m1, c1) (m2, c2) = (x, y)
  where
    x = (c1 - c2) / (m2 - m1)
    y = m1 * x + c1

movingTowards :: (Float, Float) -> Line -> Bool
movingTowards (a, b) ((x, y, z), (v, u, w)) = f a x v && f b y u
  where
    f target current vel = signum (target - current) == signum vel
