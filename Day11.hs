import Data.Array
import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

test = pt2 <$> readFile "test11.txt"
main = pt2 <$> readFile "input11.txt"

pt2 input =
  sum $
  map snd $
  distances $
  solve (parse ls, blanks (transpose ls), blanks ls)
  where ls = lines input

parse =
  map fst .
  filter (\(_, c) -> c == '#') .
  concatMap (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) .
  zip [0..] .
  map (zip [0..])

blanks =
  map fst .
  filter (all (== '.') . snd) .
  zip [0..]

distances gxs =
  [ ( (a, b)
    , ( abs (xa - xb) + abs (ya - yb) )
    )
  | (a, (xa, ya)) <- zip [1..] gxs
  , (b, (xb, yb)) <- zip [1..] gxs
  , a < b
  ]

solve (gxs, xBlanks, yBlanks) =
  zip
    (map (shift (reverse xBlanks)) (map fst gxs))
    (map (shift (reverse yBlanks)) (map snd gxs))

d = 1_000_000 - 1

shift [] x = x
shift (blank:more) x = shift more (if x > blank then x + d else x)
