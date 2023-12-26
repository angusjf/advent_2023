import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (minimumBy, transpose)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

test = pt2 <$> readFile "test17.txt"

main = readFile "input17.txt" >>= print . pt2

to2dArray :: [String] -> Array (Int, Int) Char
to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose ls)

pt2 s = solve grid S.empty ((0, 0), E, 0)
  where
    grid = fmap digitToInt $ to2dArray $ lines s

data Dir = N | E | S | W deriving (Eq, Ord, Show)

inBounds :: Array (Int, Int) Int -> ((Int, Int), Dir, Int) -> Bool
inBounds grid ((x, y), _, _) = x >= 0 && y >= 0 && x <= mx && y <= my
  where
    (_, (mx, my)) = bounds grid

turn N = [W, E]
turn E = [N, S]
turn S = [E, W]
turn W = [S, N]

move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) W = (x - 1, y)

neighbors (pos, dir, n) =
  ( do
      dir' <- turn dir
      return (move pos dir', dir', 0)
  )
    ++ [(move pos dir, dir, n + 1) | n < 2]

solve grid visited start =
  map snd $
    filter (\((pos, _, _), _) -> let (_, end) = bounds grid in pos == end) $
      M.assocs $
        dijkstra
          (\(pos, dir, n) -> grid ! pos)
          (filter (inBounds grid) . neighbors)
          S.empty
          (S.singleton start)
          (M.singleton start 0)

dijkstra _ _ _ unvisited costs | S.null unvisited = costs
dijkstra edgeCost neighbours visited unvisited costs =
  dijkstra
    edgeCost
    neighbours
    (S.insert current visited)
    (S.fromList (filter (`S.notMember` visited) (neighbours current)) `S.union` S.delete current unvisited)
    (M.unionWith min (M.fromList $ map (\e -> (e, costs M.! current + edgeCost e)) (neighbours current)) costs)
  where
    current = minimumBy (compare `on` (costs M.!)) unvisited
