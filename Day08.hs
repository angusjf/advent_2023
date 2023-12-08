import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import Data.Char
import Debug.Trace

test = pt2 <$> readFile "test08.txt"
main = pt2 <$> readFile "input08.txt"

pt2 =
  solve .
  parse .
  lines

parse :: [String] -> (String, M.Map String (String, String))
parse (instructions:_:more) = (cycle instructions, M.fromList $ map parseLine more)

parseLine input = (start, (from, to))
  where [start, from, to] = words $ filter (\c -> c == ' ' || isAlpha c || isNumber c) input

solve (is, m) = map (\start -> run start is m) starts
  where starts = filter ((== 'A') . last) $ M.keys m

run :: String -> String -> M.Map String (String, String) -> Int
run at (i:is) m =
  if (last at) == 'Z'
    then 0
    else 1 + run ((case i of { 'L' -> fst ; 'R' -> snd }) (m ! at)) is m
