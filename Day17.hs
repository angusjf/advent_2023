import Data.Array
import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace

test = pt2 <$> readFile "test17.txt"

main = pt2 <$> readFile "input17.txt"

to2dArray :: [String] -> Array (Int, Int) Char
to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose ls)

pt2 s =
  let grid = fmap digitToInt $ to2dArray $ lines s
      results = solve grid (S.singleton ((0, 0), E, 0)) S.empty (M.singleton ((0, 0), E, 0) 0)
   in M.mapKeysWith min (\(x, _, _) -> x) results -- filter (\((p, _, _), _) -> p == snd (bounds grid)) $ M.assocs results

data Dir = N | E | S | W deriving (Eq, Ord, Show)

type Visited = ((Int, Int), Dir, Int)

solve :: Array (Int, Int) Int -> S.Set Visited -> S.Set Visited -> M.Map Visited Int -> M.Map Visited Int
solve _ unvisited _ costs | S.null unvisited = costs
solve grid unvisited visited costs =
  let ((pos, dir, n), unvisited') = unconsMin unvisited (costs M.!)

      next = filter (`S.notMember` visited) $ filter (\(pos, _, _) -> inBounds (bounds grid) pos) $ neighbors (pos, dir, n)

      newCosts = M.fromList $ map cost next

      cost x@(p, _, _) = (x, costs M.! (pos, dir, n) + grid ! p)
   in solve
        grid
        (S.union (S.fromList next) unvisited')
        (S.insert (pos, dir, n) visited)
        (M.unionWith min newCosts costs)

neighbors (pos, dir, n) =
  ( do
      dir' <- turn dir
      return (move pos dir', dir', 0)
  )
    ++ [(move pos dir, dir, n + 1) | n < 2]

turn N = [W, E]
turn E = [N, S]
turn S = [E, W]
turn W = [S, N]

move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) W = (x - 1, y)

unconsMin :: S.Set Visited -> (Visited -> Int) -> (Visited, S.Set Visited)
unconsMin visited cost = S.deleteFindMin visited

inBounds ((0, 0), (mx, my)) (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my

draw ((0, 0), (mx, my)) dict =
  unlines $
    [ unwords [pad $ show (d M.! (x, y)) | x <- [0 .. mx]]
      | y <- [0 .. my]
    ]
  where
    d = M.mapKeysWith min (\(x, _, _) -> x) dict

pad s = replicate (5 - length s) ' ' ++ s

-- 0     4     5    11    15    18    23    31    32    33    39    46    49
-- 3     5     6    11    15   [20]   26    31    34    38    44    46    49
-- 6     7    11    16    17    21    26    32    39    42    44    49    53
-- 9    11    15    21    22    29    31    39    43    47    48    53    55
-- 17    16    19    25    28    33    38    46    49    55    53    56    61
-- 18    21    22    30    34    42    46    53    58    63    57    61    65
-- 24    26    27    34    42    49    54    62    66    70    67    67    71
-- 30    32    31    38    46    53    61    70    76    80    77    72    74
-- 37    38    37    41    50    56    64    75    83    88    88    81    81
-- 41    45    45    45    51    58    67    81    89    94    95    91    85
-- 46    48    48    49    55    63    69    79    86    92   103   101    88
-- 51    53    54    55    60    64    72    80    91    99   106   109    99
-- 55    56    56    58    64    71    75    81    86    92   101   109   105

-- [2]   [4]   [1]    3     4    [3]    2     3     1     1     3     2     3
-- 3     2    [1]   [5]   [4]   [5]    3     5     3     5     6     2     3
-- 3     2     5     5     2     4     5     6     5     4     2     5     4
-- 3     4     4     6     5     8     5     8     4     5     4     5     2
-- 4     5     4     6     6     5     7     8     6     7     5     3     6
-- 1     4     3     8     5     9     8     7     9     8     4     5     4
-- 4     4     5     7     8     7     6     9     8     7     7     6     6
-- 3     6     3     7     8     7     7     9     7     9     6     5     3
-- 4     6     5     4     9     6     7     9     8     6     8     8     7
-- 4     5     6     4     6     7     9     9     8     6     4     5     3
-- 1     2     2     4     6     8     6     8     6     5     5     6     3
-- 2     5     4     6     5     4     8     8     8     7     7     3     5
-- 4     3     2     2     6     7     4     6     5     5     5     3     3

