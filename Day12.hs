import Data.List
import qualified Data.Map as M
import Control.Monad.State

test = pt2 <$> readFile "test12.txt"
main = pt2 <$> readFile "input12.txt"

pt2 = map (runMemoized solve . scale . parse) . lines

scale (springs, breaks) = (concat $ intersperse "?" $ fiveX springs, concat $ fiveX breaks)
  where fiveX = take 5 . repeat 

parse :: [Char] -> (String, [Int])
parse l =
  ( springs
  , map read $ split (== ',') breaks
  )
  where (springs, _:breaks) = break (== ' ') l

solve ("", []) = return 1

solve ("", _)  = return 0

solve (springs, []) = return $ if '#' `elem` springs then 0 else 1

solve ('.':springs, breaks) = memoized solve (springs, breaks)
solve ('?':springs, breaks) = do
    x <- memoized solve ('.':springs, breaks)
    y <- memoized solve ('#':springs, breaks)
    return $ x + y

solve ('#':springs, b:breaks) = do
    if b <= length springs + 1
        && not ('.' `elem` take (b - 1) springs)
        && (b == length springs + 1 || springs !! (b - 1) /= '#')
        then memoized solve (drop b springs, breaks)
        else return 0

-- helpers --

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

memoized :: Ord x => (x -> State (M.Map x y) y) -> x -> State (M.Map x y) y
memoized f x = do
    cache <- get
    case M.lookup x cache of
        Just hit -> return hit
        Nothing -> do
            res <- f x
            modify (M.insert x res)
            return res

runMemoized :: (x -> State (M.Map x y) y) -> x -> y
runMemoized f x = evalState (f x) M.empty
