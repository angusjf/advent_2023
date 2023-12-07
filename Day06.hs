import Data.List
import Data.Char

test = pt2 <$> readFile "test06.txt"
main = pt2 <$> readFile "input06.txt"

pt1 = solve . parse 

parse :: String -> [[Int]]
parse = transpose . map (map read . tail . words) . lines

solve ([t, dist]:more) = ( [ (t,x) | x <- [0..t], t * x - x * x > dist  ]) : solve more
solve [] = []

pt2 = solve2 . parse2

parse2 :: String -> [Int]
parse2 = map (read . filter isNumber . snd . break isNumber) . lines

solve2 [t, dist] = length [ (t,x) | x <- [0..t], t * x - x * x > dist  ]

