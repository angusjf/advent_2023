import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Char
import Debug.Trace

test = pt2 <$> readFile "test16.txt"
main = pt2 <$> readFile "input16.txt"

pt2 = solve "AA" 0 [] . M.fromList . map parse . lines

parse :: String -> (String, (Int, [String]))
parse l = (valve, (read $ filter isDigit rate, map (filter (/= ',')) valves))
  where "Valve":valve:"has":"flow":rate:_tunnel:_lead:"to":_valves:valves = words l

solve :: String -> Int -> [String] -> Map String (Int, [String]) -> Int
solve pos 30 open dict = 0
solve pos t open dict =
  let
    (rate, next) = dict ! pos

    released = sum $ map (\valve -> fst (dict ! valve)) open

    moves = (map (\n -> solve n (t + 1) open dict) next) 

    future =
      maximum $
        if not (pos `elem` open) && rate /= 0
	  then (solve pos (t + 1) (pos:open) dict):moves
	  else moves
  in
    released + future
       
