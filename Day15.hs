import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as M

test = pt2 <$> readFile "test15.txt"
main = pt2 <$> readFile "input15.txt"

pt2 = 
  sum .
  concatMap (\(k, v) -> map ((k + 1) *) (zipWith (*) [1..] $ map snd $ v)) .
  M.assocs .
  foldl run M.empty .
  split (== ',') .
  init

type Dict = M.Map Int [(String, Int)]

run :: Dict -> String -> Dict

run dict i | last i == '-' = M.adjust (remove key) (hash key) dict
	where key = init i

run dict i =
   M.insertWith ins (hash key) [(key, v)] dict
  where v = read n :: Int
        (key, '=':n) = break (== '=') i

ins [(key, v)] [] = [(key, v)]
ins [(key, v)] ((k1, v1):more) =
  if key == k1
    then (key, v) : more
    else (k1, v1) : ins [(key, v)] more

remove key = filter (\(k, v) -> k /= key)

split f s =
    case break f s of
        ([], _:b) -> split f b
        ( a,  []) -> [a]
        ( a, _:b) -> [a] ++ split f b

hash = foldl (\n c -> ((n + ord c) * 17) `mod` 256) 0

