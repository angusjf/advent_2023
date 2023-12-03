import Data.List
import Data.Array
import Data.Char
import Debug.Trace

test = readFile "test03.txt" >>= print . two
main = readFile "input03.txt" >>= print . two

-- two input =
--     let grid = parse input
--         symbs = symbols grid
--         nums = numbers grid
--         nr = filter (near symbs) nums
--     in sum $ map snd nr

two input =
    let grid = parse input
        grs = gears grid
        nums = numbers grid
    in sum . map (product . map snd) $ filter (\xs -> length xs >= 2) $ map (near2 nums) grs

parse input = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat ls)
    where ls = lines input

symbols grid =
    filter (\(pos, c) -> not $ c == '.' || isNumber c) $ assocs grid

gears grid =
    filter (\(pos, c) -> c == '*') $ assocs grid

numbers :: Array (Int, Int) Char -> [([(Int, Int)], Int)]
numbers grid =
    map (\xs -> (map fst xs, read $ map snd xs)) $
    groupBy (\((x1, y1), _) ((x2, y2), _) -> x1 == x2 && (y1 == y2 - 1 || y1 == y2 - 2)) $
    filter (\(pos, c) -> isNumber c) $ assocs grid

near symbs (nums, _) = any (\(sym, _) -> any (\n -> adj n sym) nums) symbs

near2 nums (pos, '*') = (filter (\(ns, _) -> any (\n -> adj pos n) ns) nums)

adj (x1, y1) (x2, y2) = x1 ~= x2 && y1 ~= y2

a ~= b = a == b || a + 1 == b || a - 1 == b
