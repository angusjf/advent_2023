import Data.List
import Data.Maybe

test = pt2 <$> readFile "test14.txt"
main = pt2 <$> readFile "input14.txt"

ghci> fmap (load . snd . (!! (126 + 24))) main

pt2 =
  -- (\(i, g) -> (i, load g)) .
  -- afterCycles cycles .
  -- (!! 20) .
  zip [0..] .
  iterate spinCycle .
  transpose .
  lines

load = sum . concat . map calc
cycles = 1_000_000_000

floatUp :: String -> String
floatUp = concat . map (reverse . sort) . groupBy (\a b -> a `elem` "O." && b `elem` ".O" || a == '#' && b == '#')

calc =
  map fst .
  filter ((== 'O') . snd) .
  zip [1..] .
  reverse

spinCycle =
  rot90 .
  map floatUp .
  rot90 .
  map floatUp .
  rot90 .
  map floatUp .
  rot90 .
  map floatUp

rot90' = map reverse . transpose
rot90 = rot90' . rot90' . rot90'

---

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

-- 1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
--                   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7
-- z x y p q a b c d e f a b c d e f a b c d e f a b c e f a b c d

-- (6, a) (12, a) 

-- 25 `mod` 6 = 1
-- 6 + 1

afterCycles n states =
  let (a, b) = firstJust (\n -> findLoop (take n states)) [1..]
  in (a, b) --states !! (a + (n `mod` (b - a)))

firstJust f = head . mapMaybe f

findLoop :: [(Int, [String])] -> Maybe (Int, Int)
findLoop xs =
  do (i, _) <- find (\(_, h) -> n == h) haystack
     return (i, j)
  where (j, n) = last xs
        haystack = init xs
