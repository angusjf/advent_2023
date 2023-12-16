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

shw N = '^'
shw S = 'V'
shw E = '>'
shw W = '<'

data Visited = Never | X | Y | Both deriving (Eq)

pt1 s =
  let
    grid = fmap (\c -> (c, [])) $ to2dArray $ lines $ s
  in
    hashes $ solve (0, 0) E grid

hashes = length . filter (not . null . snd) . elems

pt2 s =
  let
    grid = fmap (\c -> (c, [])) $ to2dArray $ lines $ s
    ((0, 0), (maxX, maxY)) = bounds grid
  in
    map
      (\(pos, dir) -> ((pos, dir), hashes $ solve pos dir grid))
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
      

visit Both _ = trace "!" Both

solve :: (Int, Int) -> Dir -> Array (Int, Int) (Char, [Dir]) -> Array (Int, Int) (Char, [Dir])

solve (x, y) _ grid | x < 0 || y < 0 || x > maxX || y > maxY = grid
  where ((0, 0), (maxX, maxY)) = bounds grid

solve pos dir oldGrid =
  let (c, visited) = oldGrid ! pos
      grid = oldGrid // [(pos, (c, dir:visited))]
  in
  -- trace (draw oldGrid) $
  if not $ dir `elem` visited then
    case c of
      '|' -> if dir `elem` [E, W] then
                              let grid' = solve (move pos (r90c dir)) (r90c dir) grid
                                        in solve (move pos (r90a dir)) (r90a dir) grid'
             else solve (move pos dir) dir grid
      '-' -> if dir `elem` [N, S] then
                              let grid' = solve (move pos (r90c dir)) (r90c dir) grid
                                        in solve (move pos (r90a dir)) (r90a dir) grid'
             else solve (move pos dir) dir grid
      '/' -> solve (move pos (mirrorXY dir)) (mirrorXY dir) grid
      '\\' -> solve (move pos (mirrorX_Y dir)) (mirrorX_Y dir) grid

      '.' -> solve (move pos dir) dir grid
    else oldGrid

move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) W = (x - 1, y)

draw array = unlines $ transpose $ chunksOf (l + 1) $ map f $ elems array
  where ((0, 0), (_, l)) = bounds array
        f (_, []) = '.'
        f (_, [E]) = '>'
        f (_, [W]) = '<'
        f (_, [N]) = '^'
        f (_, [S]) = 'v'
        f (_, _) = '+'

chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)
