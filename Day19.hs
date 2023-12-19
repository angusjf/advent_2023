import Data.Char
import Data.List
import Data.Map qualified as M
import Debug.Trace
import Numeric (readHex)

test = pt2 <$> readFile "test19.txt"

main = pt2 <$> readFile "input19.txt"

pt2 input = sum $ map (\(a, b, c, d) -> a + b + c + d) $ filter (solve "in" (M.fromList $ map parseWorkflow workflows)) (map parsePart parts)
  where
    (workflows, _ : parts) = break null (lines input)

-- lhr{s<2678:A,x>1197:R,x>1073:R,R}

parseWorkflow input =
  let (ident, '{' : more) = break (== '{') input
      rules = split (== ',') $ init more
   in (ident, map parseRule (init rules) ++ [\_ -> Just (last rules)])

type Rule = String

parseRule :: String -> (Int, Int, Int, Int) -> Maybe String
parseRule (c : op : rest) (x, m, a, s) = if op' c' (read n) then Just next else Nothing
  where
    c' = case c of
      'x' -> x
      'm' -> m
      'a' -> a
      's' -> s
    (n, ':' : next) = break (== ':') rest
    op' = case op of
      '<' -> (<)
      '>' -> (>)
      e -> error (show (c, op, rest))

split f s =
  case break f s of
    ([], _ : b) -> split f b
    (a, []) -> [a]
    (a, _ : b) -> a : split f b

-- {x=468,m=844,a=1657,s=193}

parsePart :: String -> (Int, Int, Int, Int)
parsePart part = (x, m, a, s)
  where
    [x, m, a, s] = map read $ init $ split (not . isNumber) part

solve ident dict xmas =
  case applyAll (dict M.! ident) xmas of
    "A" -> True
    "R" -> False
    next -> solve next dict xmas

applyAll :: [a -> Maybe b] -> a -> b
applyAll (f : fs) x = case f x of
  Just y -> y
  Nothing -> applyAll fs x
