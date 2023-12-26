import Data.Array
import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Set qualified as S

test = readFile "test17.txt" >>= print . pt2

main = readFile "input17.txt" >>= print . pt2

to2dArray :: [String] -> Array (Int, Int) Char
to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose ls)

data Dir = N | S | W | E deriving (Eq, Ord, Show)

pt2 input = dijkstra grid S.empty (S.fromList [(0, ((0, 0), E)), (0, ((0, 0), S))])
  where
    grid = fmap digitToInt (to2dArray (lines input))

dijkstra :: Array (Int, Int) Int -> S.Set ((Int, Int), Dir) -> S.Set (Int, ((Int, Int), Dir)) -> Int
dijkstra grid visited unvisited
  | pos == snd (bounds grid) = len
  | S.member (pos, dir) visited = dijkstra grid visited unvisited'
  | otherwise =
      dijkstra
        grid
        (S.insert (pos, dir) visited)
        ( S.union
            ( S.fromList
                [ (len', (pos', dir'))
                  | dir' <- turn dir,
                    path <- take (maxPathLength - minPathLength + 1) $ drop minPathLength $ iterate (\path -> move dir' (head path) : path) [pos],
                    let pos' = head path,
                    inBounds grid pos',
                    let len' = len + sum (map (grid !) (init path))
                ]
            )
            unvisited'
        )
  where
    ((len, (pos, dir)), unvisited') = S.deleteFindMin unvisited

maxPathLength = 10

minPathLength = 4

inBounds grid (y, x) = y >= 0 && x >= 0 && y <= my && x <= mx where ((0, 0), (mx, my)) = bounds grid

turn W = [N, S]
turn E = [N, S]
turn _ = [W, E]

move N (x, y) = (x, y - 1)
move S (x, y) = (x, y + 1)
move W (x, y) = (x - 1, y)
move E (x, y) = (x + 1, y)
