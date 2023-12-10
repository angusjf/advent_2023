import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace

test = pt2 <$> readFile "test10.txt"
main = pt2 <$> readFile "input10.txt"

-- pick's theorem
-- r.i.p.
-- numberOfInteriorPoints = totalArea - (numberOfExteriorPoints / 2) + 1

pt2 input = area lp - length lp `div` 2 + 1
  where grid = to2dArray $ lines input
        start = fromJust $ findIndexArray (== 'S') grid
        lp = loop start D grid

data Direction = U | D | L | R deriving (Show)

loop start dir grid = start : 
  if grid ! pos == 'S'
    then []
    else loop pos next grid 
  where 
    pos = move start dir
    next =
      case (dir, grid ! pos) of
        (U, 'F') -> R
        (U, '7') -> L
        (U, '|') -> U

        (D, 'L') -> R
        (D, 'J') -> L
        (D, '|') -> D

        (R, 'J') -> U
        (R, '7') -> D
        (R, '-') -> R

        (L, 'F') -> D
        (L, 'L') -> U
        (L, '-') -> L

move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)

-- shoestring
area points = abs $ sum (zipWith f points (drop 1 (cycle points))) `div` 2
  where f (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- array helpers

to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose $ ls)

findIndexArray :: Ix i => (a -> Bool) -> Array i a -> Maybe i
findIndexArray f = fmap fst . find (f . snd) . assocs
