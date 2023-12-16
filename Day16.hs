import Data.List
import Data.Array
import Data.Complex

test = pt2 <$> readFile "test16.txt"
main = pt2 <$> readFile "input16.txt"

to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose $ ls)

pt1 =
  unvisted .
  solve (0, 0) (1 :+ 0) .
  fmap (\c -> (c, [])) .
  to2dArray .
  lines

pt2 s =
  let
    grid = fmap (\c -> (c, [])) $ to2dArray $ lines $ s
  in
    maximum [ unvisted $ solve pos dir grid
            | dir <- [0 :+ 1, 0 :+ (-1), 1 :+ 0, (-1) :+ 0] 
            , pos <- borders grid
            ]

unvisted = length . filter (not . null . snd) . elems

solve (x, y) _ grid | x < 0 || y < 0 || x > maxX || y > maxY = grid
  where ((0, 0), (maxX, maxY)) = bounds grid

solve pos dir oldGrid =
  let (c, visited) = oldGrid ! pos
      grid = oldGrid // [(pos, (c, dir:visited))]
      dirs =
        case c of
          '|' -> if realPart dir /= 0 then [dir * i, dir * (-i)] else [dir]
          '-' -> if imagPart dir /= 0 then [dir * i, dir * (-i)] else [dir]
          '/' -> [mirrorXY dir]
          '\\' -> [mirrorX_Y dir]
          '.' -> [dir]
  in
  if not $ dir `elem` visited then
    foldr (\d g -> solve (move pos d) d g) grid dirs
    else oldGrid

i = 0 :+ 1

mirrorX_Y (a :+ b) = b :+ a

mirrorXY (a :+ b) = (-b) :+ (-a)

move (x, y) (a :+ b) = (x + floor a, y + floor b)

borders grid = concat [ [ (0,    y) | y <- [0 .. maxY] ]
                      , [ (maxX, y) | y <- [0 .. maxY] ]
                      , [ (x,    0) | x <- [0 .. maxX] ]
                      , [ (x, maxY) | x <- [0 .. maxX] ]
                      ]
  where ((0, 0), (maxX, maxY)) = bounds grid
