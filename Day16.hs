import Data.List
import Data.Char
import Data.Array
import Debug.Trace

test = pt2 <$> readFile "test16.txt"
main = pt2 <$> readFile "input16.txt"

to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose $ ls)

data Dir = N | E | S | W deriving (Enum, Show, Eq)

r90c W = N
r90c x = succ x

r90a N = W
r90a x = pred x

mirrorXY N = E
mirrorXY E = N
mirrorXY S = W
mirrorXY W = S

mirrorX_Y N = W
mirrorX_Y W = N
mirrorX_Y S = E
mirrorX_Y E = S

move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) W = (x - 1, y)

pt1 s =
  let grid = fmap (\c -> (c, [])) $ to2dArray $ lines $ s
  in unvisted $ solve (0, 0) E grid

unvisted = length . filter (not . null . snd) . elems

pt2 s =
  let
    grid = fmap (\c -> (c, [])) $ to2dArray $ lines $ s
    ((0, 0), (maxX, maxY)) = bounds grid
  in
    maximum $
    map snd $
    map
      (\(pos, dir) -> ((pos, dir), unvisted $ solve pos dir grid))
      ( concat [ [ ((0, y), dir)
                 | y <- [0 .. maxY]
                 , dir <- [N, S, E, W]
                 ]
               , [ ((maxX, y), dir)
                 | y <- [0 .. maxY]
                 , dir <- [N, S, E, W]
                 ]
               , [ ((x, 0), dir)
                 | x <- [0 .. maxX]
                 , dir <- [N, S, E, W]
                 ]
               , [ ((x, maxY), dir)
                 | x <- [0 .. maxX]
                 , dir <- [N, S, E, W]
                 ]
               ])
      

solve :: (Int, Int) -> Dir -> Array (Int, Int) (Char, [Dir]) -> Array (Int, Int) (Char, [Dir])

solve (x, y) _ grid | x < 0 || y < 0 || x > maxX || y > maxY = grid
  where ((0, 0), (maxX, maxY)) = bounds grid

solve pos dir oldGrid =
  let (c, visited) = oldGrid ! pos
      grid = oldGrid // [(pos, (c, dir:visited))]
      dirs =
        case c of
          '|' -> if dir `elem` [E, W] then [r90c dir, r90a dir] else [dir]
          '-' -> if dir `elem` [N, S] then [r90c dir, r90a dir] else [dir]
          '/' -> [mirrorXY dir]
          '\\' -> [mirrorX_Y dir]
          '.' -> [dir]
  in
  if not $ dir `elem` visited then
    foldr (\d g -> solve (move pos d) d g) grid dirs
    else oldGrid
