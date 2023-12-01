import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

test = readFile "test01.txt" >>= putStrLn . pt2
main = readFile "input01.txt" >>= putStrLn . pt2

pt2 = show
    . sum
    . map ( (\ns -> 10 * head ns + last ns)
          . mapMaybe (`Map.lookup` n)
          . sublists
          )
    . lines

sublists = concatMap inits . tails

n = Map.fromList $ [ ("one", 1)
                   , ("two", 2)
                   , ("three", 3)
                   , ("four", 4)
                   , ("five", 5)
                   , ("six", 6)
                   , ("seven", 7)
                   , ("eight", 8)
                   , ("nine", 9)
                   ] ++ [ (show d, d) | d <- [1..9] ]
