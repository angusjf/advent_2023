import Data.List
import Data.Array

test = pt2 <$> readFile "test14.txt"
main = pt2 <$> readFile "input14.txt"

pt2 =
  sum .
  concat .
  map calc .
  map floatUp .
  transpose .
  lines

cycles = 1_000_000_000

floatUp :: String -> String
floatUp = concat . map (reverse . sort) . groupBy (\a b -> a `elem` "O." && b `elem` ".O" || a == '#' && b == '#')

calc =
  map fst .
  filter ((== 'O') . snd) .
  zip [1..] .
  reverse

---

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b
