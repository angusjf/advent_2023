import Data.List
import Data.Array

test = pt2 <$> readFile "test13.txt"
main = pt2 <$> readFile "input13.txt"

pt2 = total . map solve . split null . lines

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

data Fold = NoFold | Horizontal Int | Vertical Int deriving (Show)

solve grid =
    case find (sym grid) [1..length grid - 1] of
      Just x -> Horizontal x
      Nothing ->
        case find (sym (transpose grid)) [1..length (head grid) - 1] of
	  Just x -> Vertical x
	  Nothing -> NoFold

sym grid n = all id $ zipWith (==) a b
  where (a, b) = reflect n grid

reflect n xs = (drop (la -| lb) a, drop (lb -| la) $ reverse b)
  where (a, b) = splitAt n xs
        l = min la lb
	la = length a
	lb = length b

a -| b = max 0 (a - b)

total folds = sum (map (\(Horizontal x) -> x) horiz) * 100 + sum (map (\(Vertical y) -> y) vert)
  where (horiz, vert) = partition isHorizontal folds
        isHorizontal (Horizontal _) = True
        isHorizontal (Vertical _) = False
