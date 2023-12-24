import Data.Array
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace

test = pt2 <$> readFile "test23.txt"

main = pt2 <$> readFile "input23.txt"

to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose ls)

data Dir = N | E | S | W deriving (Eq, Ord, Show)

pt2 = solve (1, 0) S.empty . to2dArray . lines

solve :: (Int, Int) -> S.Set (Int, Int) -> Array (Int, Int) Char -> Int
solve (x, y) visited grid | (x, y) == (mx - 1, my) = 0
  where
    ((0, 0), (mx, my)) = bounds grid
solve next visited grid =
  1
    + maximum
      [ solve p (S.insert next visited) grid
        | (dir, p@(nx, ny)) <- adj next,
          S.notMember p visited,
          nx >= 0 && ny >= 0 && nx <= mx && ny <= my,
          compatible dir (grid ! p)
      ]
  where
    ((0, 0), (mx, my)) = bounds grid

adj (x, y) = [(S, (x, y + 1)), (N, (x, y - 1)), (W, (x - 1, y)), (E, (x + 1, y))]

deleteFindMin :: S.Set ((Dir, (Int, Int)), Int) -> (((Dir, (Int, Int)), Int), S.Set ((Dir, (Int, Int)), Int))
deleteFindMin visited = (head l, S.fromList (tail l))
  where
    l = sortBy (\((_, _), a) ((_, _), b) -> compare b a) $ S.toList visited

compatible _ '.' = True
compatible _ '#' = False
compatible N '^' = True
compatible E '>' = True
compatible W '<' = True
compatible S 'v' = True
compatible _ _ = False
