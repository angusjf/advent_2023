import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace

test = pt2 <$> readFile "test24.txt"

main = pt2 <$> readFile "input24.txt"

pt2 = map fst . map parse . lines

parse :: String -> Line
parse line = let [x, y, z, _, v, u, w] = map read $ words $ filter (/= ',') line in ((x, y, z), (v, u, w))

mn = 200000000000000

mx = 400000000000000

type Line = ((Int, Int, Int), (Int, Int, Int))

solve lines = lines
