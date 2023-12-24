import Control.Monad.State
import Data.Array
import Data.List (find, transpose)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set, empty, insert, member, notMember, singleton, size)
import Data.Set qualified as S
import Debug.Trace

test = pt2 <$> readFile "test21.txt"

main = pt2 <$> readFile "input21.txt"

to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose ls)

pt2 = answer . to2dArray . lines

answer grid = draw grid $ runMemoized f (500, findS grid)
  where
    -- f :: (Int, (Int, Int)) -> State (M.Map (Int, Int) (Set (Int, Int))) (Set (Int, Int))
    f (0, pos) = return $ singleton pos
    f (n, pos) = mconcat <$> mapM (\p -> memoized (bounds grid) f (n - 1, p)) (neighbours grid pos)

findS = fst . fromJust . find ((== 'S') . snd) . assocs

neighbours grid (x, y) =
  [ pos
    | (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)],
      let pos = (x + dx, y + dy),
      let fake = ((x + dx) ~ mx, (y + dy) ~ my),
      inBounds fake,
      grid ! fake /= '#'
  ]
  where
    ((0, 0), (mx, my)) = bounds grid
    inBounds (x, y) = x >= 0 && y >= 0 && x <= mx && y <= my

draw :: Array (Int, Int) Char -> Set (Int, Int) -> [String]
draw grid visited =
  show (length visited)
    : [ [ if (x, y) `member` visited then 'O' else grid ! (x, y)
          | x <- [0 .. mx]
        ]
        | y <- [0 .. my]
      ]
  where
    ((0, 0), (mx, my)) = bounds grid

---

-- lg (oldx, wrappedx, xx) x@(newxx, _) = trace (show oldx ++ " was mapped to" ++ show wrappedx ++ " so mapping " ++ show xx ++ " to " ++ show newxx) x
lg (oldx, wrappedx, xx) x@(newxx, _) = x

memoized ((0, 0), (mx, my)) f x' = do
  cache <- get
  case M.lookup x cache of
    Just hit -> return hit
    Nothing -> do
      res <- f x
      modify (M.insert x res)
      return (S.map (\(xx, yy) -> lg (oldx, wrappedx, xx) (xx + (oldx - wrappedx), yy + (oldy - wrappedy))) res)
  where
    (n, (oldx, oldy)) = x'
    wrappedx = oldx ~ mx
    wrappedy = oldy ~ my
    x = (n, (wrappedx, wrappedy))

runMemoized f x = evalState (f x) M.empty

(~) :: Int -> Int -> Int
x ~ n =
  if x < 0
    then (n + x + 1) `mod` (n + 1)
    else x `mod` (n + 1)
