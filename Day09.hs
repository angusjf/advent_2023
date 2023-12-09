import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import Data.Char
import Debug.Trace

test = pt2 <$> readFile "test09.txt"
main = pt2 <$> readFile "input09.txt"

pt2 =
  -- sum . 
  map (solve . map read . words) .
  lines

solve xs = foldr1 (-) $ map head $ triangulate xs

triangulate xs = takeWhile (not . all (== 0)) $ iterate diffs xs

diffs xs = zipWith (flip (-)) xs (tail xs)
