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
   in draw (bounds grid) results -- filter (\((p, _, _), _) -> p == snd (bounds grid)) $ M.assocs results

data Dir = N | E | S | W deriving (Eq, Ord, Show)

type Visited = ((Int, Int), Dir, Int)

solve :: Array (Int, Int) Int -> S.Set Visited -> S.Set Visited -> M.Map Visited Int -> M.Map Visited Int
solve _ unvisited _ costs | S.null unvisited = costs
solve grid unvisited visited costs =
  let ((pos, dir, n), unvisited') = unconsMin unvisited (costs M.!)

      next = filter (`S.notMember` visited) $ filter (\(pos, dir, n) -> inBounds (bounds grid) pos) $ neighbors (pos, dir, n)

      newCosts = M.fromList $ map cost next

      cost x@(p, _, _) = (x, costs M.! (pos, dir, n) + grid ! p)
   in solve
        grid
        (S.union unvisited' $ S.fromList next)
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
