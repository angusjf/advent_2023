import Data.List
import Data.Map qualified as M
import Debug.Trace
import Numeric (readHex)

test = pt2 <$> readFile "test18.txt"

-- main = pt2 <$> readFile "input18.txt"
main = pt2 <$> readFile "input18.txt"

-- pick's theorem
-- r.i.p.
-- numberOfInteriorPoints = totalArea - (numberOfExteriorPoints / 2) + 1
-- (\lp -> area lp - length lp `div` 2 + 1)

pt2 input = i + b
  where
    lp = lps input
    i = dbg "i" $ a - b `div` 2 + 1
    b = dbg "b" $ boundary (head lp) lp
    a = dbg "a" $ area lp

dbg s x = trace (s ++ ": " ++ show x) x

lps =
  map fst
    . snd
    . foldr (run . parse) ((0, 0), [])
    . lines

parse (c : ' ' : more) = (c, fst $ head $ readHex hex, "")
  where
    (n, ' ' : '(' : '#' : hexcode') = break (== ' ') more
    hex = take 5 hexcode'
    c = digitToChar $ hexcode' !! 5

digitToChar '0' = 'R'
digitToChar '1' = 'D'
digitToChar '2' = 'L'
digitToChar '3' = 'U'

run (dir, n, code) (pos, dict) = (move pos dir n, (pos, code) : dict)

move (x, y) 'U' n = (x, y + n)
move (x, y) 'D' n = (x, y - n)
move (x, y) 'L' n = (x - n, y)
move (x, y) 'R' n = (x + n, y)

draw dict =
  map (\y -> [if M.member (x, y) dict then '#' else '.' | x <- [minx .. maxx]]) [miny .. maxy]
  where
    d = map fst $ M.assocs dict
    minx = minimum $ map fst d
    maxx = maximum $ map fst d
    miny = minimum $ map snd d
    maxy = maximum $ map snd d

-- shoestring
area points = abs $ sum (zipWith f points (drop 1 (cycle points))) `div` 2
  where
    f (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

boundary (x2, y2) [(x1, y1)] | x1 == x2 = max y1 y2 - min y1 y2
boundary (x2, y2) [(x1, y1)] | y1 == y2 = max x1 x2 - min x1 x2
boundary m ((x1, y1) : (x2, y2) : more) | x1 == x2 = (max y1 y2 - min y1 y2) + boundary m ((x2, y2) : more)
boundary m ((x1, y1) : (x2, y2) : more) | y1 == y2 = (max x1 x2 - min x1 x2) + boundary m ((x2, y2) : more)
