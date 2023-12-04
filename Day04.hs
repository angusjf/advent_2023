import Data.List

test = readFile "test04.txt" >>= print . two
main = readFile "input04.txt" >>= print . two

two =
    sum .
    map fst .
    f .
    zip (repeat 1) .
    map (length . uncurry intersect . parse) .
    lines

parse :: String -> ([Int], [Int])
parse ('C':'a':'r':'d':' ':more) = (map read $ words $ winners, map read $ words $  have)
  where (n,    _:numbers) = break (== ':') more
        (winners, _:have) = break (== '|') numbers

f ((n, x):more) = (n, x) : f ( (map (\(nn, xx) -> (nn + n, xx)) a) ++ b )
  where (a, b) = splitAt x more
f [] = []
