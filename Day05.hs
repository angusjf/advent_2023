import Data.List
import Debug.Trace
import Data.Maybe

test = pt2 <$> readFile "test05.txt"
main = pt2 <$> readFile "input05.txt"

pt2 =
  sort .
  map fst .
  uncurry solve .
  parse

parse :: String -> ([[[Int]]], [(Int, Int)])
parse input = (map parseMap maps, pairs $ parseSeeds seeds)
  where (seeds:maps) = map lines $ split (breakOn "\n\n") input

parseSeeds [input] = map read $ words ns
  where (_, _:_:ns) = break (== ':') input

parseMap (title:more) = (map (map read . words) more)
  where (name, " map:") = break (== ' ') title
        (from, _:_:_:_:to) = break (== '-') name

split f s =
    case f s of
        ([],  b) -> split f b
        ( a, []) -> [a]
        ( a,  b) -> [a] ++ split f b

breakOn needle haystack | needle `isPrefixOf` haystack = ([], drop (length needle) haystack)
breakOn needle [] = ([], [])
breakOn needle (x:xs) = (x:a, b)
  where (a, b) = breakOn needle xs

pairs :: [a] -> [(a, a)]
pairs (x:y:more) = (x, y) : pairs more
pairs [] = []

solve :: [[[Int]]] -> [(Int, Int)] -> [(Int, Int)]
solve [] (seeds) = filter (\(_, n) -> n > 0) seeds
solve (maps:more) (seeds) = solve more (concatMap (applyMaps maps) seeds)

applyMaps _ (_, 0) = []
applyMaps [] seed = [seed]
applyMaps ([dstStart, srcStart, rangeLen]:more) (start, len) =
  let
    (before, inside, after) = cut (srcStart, rangeLen) (start, len)
    (inStart, inLen) = inside
    shiftedInside = (inStart - srcStart + dstStart, inLen)
  in
    concat [applyMaps more before, [shiftedInside], applyMaps more after]

cut :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int), (Int, Int))
cut src range =
  (before src range, inside src range, after src range)

after1 (srcStart, srcLen) (start, len) = 
  (max start (srcStart + srcLen), max 0 $ min len ((start + len) - (srcStart + srcLen)))

after2 a b = let (s, d) = after1 a b in if d <= 0 then (0, 0) else (s, d)

inside1 (a, b) (c, d) | a < 0 || b < 0 || c < 0 || d < 0 = (0, 0)
inside1 (srcStart, srcLen) (start, len) =
      ( max (srcStart) (min (srcStart + srcLen - 1) start)
      , min len $ min
          ((start + len) - srcStart)
          ((srcStart + srcLen) - start)
      )

inside2 a b = let (s, d) = inside1 a b in if d <= 0 then (0, 0) else (s, d)

before1 (srcStart, srcLen) (start, len) = 
  (start, min len (srcStart - start))

before2 a b = let (s, d) = before1 a b in if d <= 0 then (0, 0) else (s, d)

before = before2
after = after2
inside = inside2

