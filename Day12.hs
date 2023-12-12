import Data.List
import qualified Data.Map as M
import Control.Monad.State

test = pt2 <$> readFile "test12.txt"
main = pt2 <$> readFile "input12.txt"

pt2 = map (uncurry solveWithCache . scale . parse) . lines

scale (springs, breaks) = (concat $ intersperse "?" $ fiveX springs, concat $ fiveX breaks)
  where fiveX = take 5 . repeat 

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

solveCache springs breaks = do
    cache <- get
    case M.lookup (springs, breaks) cache of
        Just x -> return x
        Nothing -> do
            res <- solve springs breaks
            modify (M.insert (springs, breaks) res)
            return res

solve "" [] = return 1

solve "" _  = return 0

solve springs [] = return $ if '#' `elem` springs then 0 else 1

solve ('.':springs) breaks = solveCache springs breaks
solve ('?':springs) breaks = do
    x <- solveCache ('.':springs) breaks
    y <- solveCache ('#':springs) breaks
    return $ x + y

solve ('#':springs) (b:breaks) = do
    if b <= length springs + 1
        && not ('.' `elem` take (b - 1) springs)
        && (b == length springs + 1 || springs !! (b - 1) /= '#')
        then solveCache (drop b springs) breaks
        else return 0

solveWithCache :: String -> [Int] -> Int
solveWithCache springs breaks = evalState (solve springs breaks) M.empty
