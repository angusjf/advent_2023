import Data.List

test = readFile "test04.txt" >>= print . two
main = readFile "input04.txt" >>= print . two

two =
    sum .
    map fst .
    f .
    map (\x -> (1, x)) .
    map length . 
    map (\(winners, have) -> intersect winners have) .
    map parse .
    lines

parse :: String -> ([Int], [Int])
parse ('C':'a':'r':'d':' ':more) = (map read $ words winners, map read $ words have)
  where (n, ':':numbers) = break (== ':') more
        (winners, '|':have) = break (== '|') numbers

f ((n, x):more) = (n, x) : f ( (map (\(nn, xx) -> (nn + n, xx)) (take x more)) ++ (drop x more) )
f [] = []
