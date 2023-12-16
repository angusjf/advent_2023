import Data.List
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.Complex
import Data.Foldable
import Data.Set (Set, empty)
import qualified Data.Set as Set

test1 = readFile "test16.txt" >>= pt1
main1 = readFile "input16.txt" >>= pt1

test = readFile "test16.txt" >>= pt2
main = readFile "input16.txt" >>= pt2 >>= putStrLn . unlines . map show

to2dArray :: [String] -> Array (Int, Int) Char
to2dArray ls = listArray ((0, 0), (length (head ls) - 1, length ls - 1)) (concat $ transpose $ ls)

pt1 s = run (0, 0) E (to2dArray (lines s))

pt2 s = do
   let grid = to2dArray (lines s)
   let ((0, 0), (maxX, maxY)) = bounds grid
   concat <$> mapM
     (\(poss, dir) -> mapM (\pos -> run pos dir grid) poss)
     [ ([ (0,    y) | y <- [0 .. maxY] ], E)
     , ([ (maxX, y) | y <- [0 .. maxY] ], W)    
     , ([ (x,    0) | x <- [0 .. maxX] ], S)
     , ([ (x, maxY) | x <- [0 .. maxX] ], N)
     ]
                                 
run pos dir grid = newArray (bounds grid) empty >>= solve pos dir grid >>= unvisted                   
                                 
unvisted :: (IOArray (Int, Int) (Set Dir)) -> IO Int
unvisted a = do
               es <- getElems a
               return $ length $ filter (not . null) es

solve :: (Int, Int) -> Dir -> Array (Int, Int) Char -> IOArray (Int, Int) (Set Dir) -> IO (IOArray (Int, Int) (Set Dir))

solve (x, y) _ grid v | x < 0 || y < 0 || x > maxX || y > maxY = return v
  where ((0, 0), (maxX, maxY)) = bounds grid

solve pos@(x, y) dir grid visits = do
      visited <- readArray visits pos 
      if not $ dir `elem` visited
        then do
          () <- writeArray visits pos (Set.insert dir visited)
          let dirs = case grid ! pos of
                       '|' -> if dir == E || dir == W then [r90a dir, r90c dir] else [dir]
                       '-' -> if dir == N || dir == S then [r90a dir, r90c dir] else [dir]
                       '/' -> [mirrorXY dir]
                       '\\' -> [mirrorX_Y dir]
                       '.' -> [dir]
          () <- mapM_ (\d -> solve (move pos d) d grid visits) dirs
          return visits
        else return visits

data Dir = N | E | S | W deriving (Eq, Ord)

r90c W = N
r90c N = E
r90c E = S
r90c S = W

r90a N = W
r90a E = N
r90a S = E
r90a W = S
  
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
