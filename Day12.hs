import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import Debug.Trace

test = pt2 <$> readFile "test12.txt"
main = pt2 <$> readFile "input12.txt"

-- pt1 input = map (uncurry solve . parse) $ lines input

pt2 input = map (fst . uncurry (solveCache M.empty) . scale . parse) $ lines input

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

type Cache = M.Map (String, [Int]) Int

solveCache  :: Cache -> String -> [Int] -> (Int, Cache)
solveCache dict springs breaks =
    case M.lookup (springs, breaks) dict of
	Just x -> (x, dict)
	Nothing ->
	    let
	      (res, dict') = solve dict springs breaks
	      dict'' = M.insert (springs, breaks) res dict'
	    in
	      (res, dict'')

solve :: Cache -> String -> [Int] -> (Int, Cache)
solve c "" [] = (1, c)
solve c "" _  = (0, c)

solve c springs [] = (if '#' `elem` springs then 0 else 1, c)

solve c ('.':springs) breaks = solveCache c springs breaks

solve c ('?':springs) breaks = (x + y, dict'')
  where (x, dict') = solveCache c ('.':springs) breaks
        (y, dict'') = solveCache dict' ('#':springs) breaks

solve c ('#':springs) (b:breaks) =
        if b <= length springs + 1
	&& not ('.' `elem` take (b - 1) springs)
	&& (b == length springs + 1 || springs !! (b - 1) /= '#')
	  then solveCache c (drop b springs) breaks
          else (0, c)
