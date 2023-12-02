import Data.List
import Data.Char
import Debug.Trace

test = readFile "test02.txt" >>= print . two
main = readFile "input02.txt" >>= print . two

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

data Color = Red | Blue | Green deriving (Show, Eq)

parse "red" = Red
parse "blue" = Blue
parse "green" = Green

type Game = (Int, [Round])

type Round = [(Int, Color)]

pair :: String -> (Int, Color)
pair (' ':s) = (read n, parse color)
  where (n, ' ':color) = break (== ' ') s

line :: String -> [Round]
line = map (map pair . split (== ',')) . split (== ';') . tail . dropWhile (/= ':') 

one = sum . map fst . filter (wouldBePossibleWith 12 13 14) . zip[1..] . map line . lines

wouldBePossibleWith :: Int -> Int -> Int -> Game -> Bool
wouldBePossibleWith r g b (_, rounds) =
    all (valid r g b) rounds

valid r g b round = all (\(n, c) -> 
        n <= (case c of
            Red -> r
            Green -> g
            Blue -> b)
    ) round

two = sum . map power . zip[1..] . map line . lines

power (_, rounds) =
    (f Red rounds * f Green rounds * f Blue rounds)

f :: Color -> [Round] -> Int
f col =
    maximum . concatMap (map (\(n, c) -> if c == col then n else 0))
    

