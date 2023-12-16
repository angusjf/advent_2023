import Data.List
import Data.Array.MArray
import Data.Array.IO
import Data.Complex
import Data.Foldable

test1 = readFile "test16.txt" >>= pt1
main1 = readFile "input16.txt" >>= pt1

test = readFile "test16.txt" >>= pt2
main = readFile "input16.txt" >>= pt2 >>= putStrLn . unlines . map show

to2dArray :: [String] -> IO (IOArray (Int, Int) Char)
to2dArray ls = newListArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose $ ls)

pt1 :: String -> IO Int
pt1 s = do
  a <- to2dArray (lines s)
  a' <- mapArray (\c -> (c, [])) a
  solved <- solve (0, 0) (1 :+ 0) a'
  unvisted solved

copy :: IOArray (Int, Int) (Char, [Complex Float]) -> IO (IOArray (Int, Int) (Char, [Complex Float]))
copy = mapArray id

pt2 s = do
   a <- to2dArray (lines s)
   bounds <- getBounds a
   a' <- mapArray (\c -> (c, [])) a
   sequence [ copy a' >>= solve pos dir >>= unvisted
            | dir <- [0 :+ 1, 0 :+ (-1), 1 :+ 0, (-1) :+ 0] 
            , pos <- borders bounds
            ]

unvisted :: (IOArray (Int, Int) (Char, [Complex Float])) -> IO Int
unvisted a = do
               es <- getElems a
               return $ length $ filter (not . null . snd) es

solve ::(Int, Int) -> Complex Float -> IOArray (Int, Int) (Char, [Complex Float]) -> IO ( IOArray (Int, Int) (Char, [Complex Float]) )
solve pos@(x, y) dir oldGrid =
  do
    ((0, 0), (maxX, maxY)) <- getBounds oldGrid
    if x < 0 || y < 0 || x > maxX || y > maxY then
      return oldGrid
    else do
      (c, visited) <- readArray oldGrid pos 
      if not $ dir `elem` visited
        then do
          () <- writeArray oldGrid pos (c, dir:visited)
          let dirs = case c of
                       '|' -> if realPart dir /= 0 then [dir * i, dir * (-i)] else [dir]
                       '-' -> if imagPart dir /= 0 then [dir * i, dir * (-i)] else [dir]
                       '/' -> [mirrorXY dir]
                       '\\' -> [mirrorX_Y dir]
                       '.' -> [dir]
          let xs = map (\d -> solve (move pos d) d oldGrid) dirs
          last <$> sequence xs
        else return oldGrid

i = 0 :+ 1

mirrorX_Y (a :+ b) = b :+ a

mirrorXY (a :+ b) = (-b) :+ (-a)

move (x, y) (a :+ b) = (x + floor a, y + floor b)

borders ((0, 0), (maxX, maxY)) = concat [ [ (0,    y) | y <- [0 .. maxY] ]
                      , [ (maxX, y) | y <- [0 .. maxY] ]
                      , [ (x,    0) | x <- [0 .. maxX] ]
                      , [ (x, maxY) | x <- [0 .. maxX] ]
                      ]
