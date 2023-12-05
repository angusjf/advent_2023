import Data.List
import Debug.Trace
import Data.Maybe

test = readFile "test05.txt" >>= print . two
main = readFile "input05.txt" >>= print . two

two =
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

----------

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

shead :: [a] -> Maybe a
shead [] = Nothing
shead (a:_) = Just a


-- cut :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int), (Int, Int))
-- cut (srcStart, srcLen) (start, len) =
--   let
--       src = [srcStart..srcStart + srcLen - 1]
--       seeds = [start..start + len - 1]
  
--       before = filter (\s -> s < srcStart) seeds
--       inside = intersect src seeds
--       after = 
  
--   in ( (fromMaybe 0 $ shead before, length before)
--      , (fromMaybe 0 $ shead inside, length inside)
--      , ( after, length after)
--      )

cut :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int), (Int, Int))
cut src range =
  (before src range, inside src range, after src range)

f xs = (fromMaybe 0 $ shead xs, length xs)

-- [(82,3),(46,10),(60,1),(86,4),(94,3),(56,4),(97,2)]
-- [(82,3),(46,10),(60,1),(86,4),(95,2),(57,3),(98,4)]

after0 (srcStart, srcLen) (start, len) = f $ filter (\s -> s >= srcStart + srcLen) seeds
  where src = [srcStart..srcStart + srcLen - 1]
        seeds = [start..start + len - 1]
after1 (srcStart, srcLen) (start, len) = 
  (max start (srcStart + srcLen), max 0 $ min len ((start + len) - (srcStart + srcLen)))

after2 a b = let (s, d) = after1 a b in if d <= 0 then (0, 0) else (s, d)

inside0 (srcStart, srcLen) (start, len) = f $ intersect src seeds
  where src = [srcStart..srcStart + srcLen - 1]
        seeds = [start..start + len - 1]

inside1 (a, b) (c, d) | a < 0 || b < 0 || c < 0 || d < 0 = (0, 0)
inside1 (srcStart, srcLen) (start, len) =
      ( max (srcStart) (min (srcStart + srcLen - 1) start)
      , min len $ min
          ((start + len) - srcStart)
          ((srcStart + srcLen) - start)
      )

inside2 a b = let (s, d) = inside1 a b in if d <= 0 then (0, 0) else (s, d)

before0 (srcStart, srcLen) (start, len) = f $ filter (\s -> s < srcStart) seeds
  where src = [srcStart..srcStart + srcLen - 1]
        seeds = [start..start + len - 1]
  
before1 (srcStart, srcLen) (start, len) = 
  (start, min len (srcStart - start))

before2 a b = let (s, d) = before1 a b in if d <= 0 then (0, 0) else (s, d)

check n f g a b = let x = g a b
                      y = f a b 
                  in if x == y then x else error $ n ++ show (a, b, x, y)


before = before2 -- check "before" before0 before2
after = after2 -- check "after" after0 after2
inside = inside2 -- check "inside " inside0 inside2

