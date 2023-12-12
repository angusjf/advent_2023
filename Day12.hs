import Data.Array
import Data.List
import Data.Maybe
import Data.Tuple
import Debug.Trace

test = pt2 <$> readFile "test12.txt"
main = pt2 <$> readFile "input12.txt"

pt1 input = map (uncurry solve . parse) $ lines input

pt2 input = map (uncurry solve . scale . parse) $ lines input

scale (springs, breaks) = (concat $ intersperse "?" $ take 5 $ repeat springs, concat $ take 5 $ repeat breaks)

parse :: [Char] -> (String, [Int])
parse l =
  ( springs
  , map read $ split (== ',') breaks
  )
  where (springs, _:breaks) = break (== ' ') l

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

solve "" [] = 1
solve "" _  = 0

solve springs [] = if '#' `elem` springs then 0 else 1

solve ('.':springs) breaks = solve springs breaks

solve ('?':springs) breaks = solve ('.':springs) breaks + solve ('#':springs) breaks

solve ('#':sprongs) (b:breaks) =
        if b <= length sprongs + 1
	&& not ('.' `elem` take (b - 1) sprongs)
	&& (b == length sprongs + 1 || sprongs !! (b - 1) /= '#')
	  then solve (drop b sprongs) breaks
          else 0
