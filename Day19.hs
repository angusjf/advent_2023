-- stack dict (Always "in")

import Data.Char
import Data.List
import Data.Map qualified as M
import Debug.Trace
import Numeric (readHex)

test = pt2 <$> readFile "test19.txt"

main = pt2 <$> readFile "input19.txt"

pt2 input = stack dict (Always "in")
  where
    (workflows, _ : parts) = break null (lines input)
    xmas = [('x', [Always "id"]), ('m', [Always "id"]), ('a', [Always "id"]), ('s', [Always "id"])]
    dict = M.fromList $ map parseWorkflow workflows

-- lhr{s<2678:A,x>1197:R,x>1073:R,R}

parseWorkflow input =
  let (ident, '{' : more) = break (== '{') input
      rules = split (== ',') $ init more
   in (ident, map (Conditional . parseRule) (init rules) ++ [Always (last rules)])

data Rule = Conditional (Char, Op, Int, String) | Always String deriving (Show)

data Op = LessThan | GreaterThan deriving (Show)

parseRule :: String -> (Char, Op, Int, String)
parseRule (c : op : rest) = (c, op', read n, next)
  where
    (n, ':' : next) = break (== ':') rest
    op' = case op of
      '<' -> LessThan
      '>' -> GreaterThan
      e -> error (show (c, op, rest))

split f s =
  case break f s of
    ([], _ : b) -> split f b
    (a, []) -> [a]
    (a, _ : b) -> a : split f b

data T = Node [T] | Accept | Reject | Fork Char Op Int T deriving (Show)

stack dict rule =
  case rule of
    Always "A" -> Accept
    Always "R" -> Reject
    Always id -> Node $ map (stack dict) (dict M.! id)
    Conditional (c, op, n, next) -> Fork c op n $ stack dict (Always next)

solve Accept = (4000, 4000, 4000, 4000)
solve Reject = (0, 0, 0, 0)
solve (Node xs) = foldl1' combine (map solve xs)

-- solve (Fork 'x' op n next) =

combine (a, b, c, d) (x, y, z, q) = (a, b, c, d)
