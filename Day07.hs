import Data.List
import qualified Data.Map as M
import Data.Char

test = pt2 <$> readFile "test07.txt"
main = pt2 <$> readFile "input07.txt"

pt2 =
  sum .
  map (uncurry (*)) .
  zip [1..] .
  map snd .
  reverse .
  sortBy cmpWithBets .
  -- unlines .
  -- map show .
  -- map (\(c, _) -> (reverse $ sortOn snd $ M.toList $ count c, maxHand c)) .
  map (parse . words) .
  lines

cmpWithBets (hand1, _) (hand2, _) = cmp hand1 hand2

-- data Card = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 deriving (Eq, Ord, Show)
data Card = A | K | Q | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | J deriving (Eq, Ord, Show)

parse :: [String] -> ([Card], Int)
parse [hand, bid] = (map card hand, read bid)

data Hand
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving (Show, Eq, Ord)

cmp :: [Card] -> [Card] -> Ordering
cmp a b =
  case compare (maxHand a) (maxHand b) of
    LT -> LT
    GT -> GT
    EQ -> compare a b

maxHand :: [Card] -> Hand
maxHand cs =
  case reverse $ sortOn snd $ M.toList $ count cs of
    [(c, _)] -> FiveOfAKind

    [(J, 4), (_, 1)] -> FiveOfAKind
    [(_, 4), (J, 1)] -> FiveOfAKind
    [(_, 4), (_, 1)] -> FourOfAKind

    [(J, 3), (_, 2)] -> FiveOfAKind
    [(_, 3), (J, 2)] -> FiveOfAKind
    [(_, 3), (_, 2)] -> FullHouse

    [(J, 3), (_, 1), (_, 1)] -> FourOfAKind
    [(_, 3), (J, 1), (_, 1)] -> FourOfAKind
    [(_, 3), (_, 1), (J, 1)] -> FourOfAKind
    [(_, 3), (_, 1), (_, 1)] -> ThreeOfAKind

    [(J, 2), (_, 2), (_, 1)] -> FourOfAKind
    [(_, 2), (J, 2), (_, 1)] -> FourOfAKind
    [(_, 2), (_, 2), (J, 1)] -> FullHouse
    [(_, 2), (_, 2), (_, 1)] -> TwoPair

    [(J, 2), (_, 1), (_, 1), (_, 1)] -> ThreeOfAKind
    [(_, 2), (J, 1), (_, 1), (_, 1)] -> ThreeOfAKind
    [(_, 2), (_, 1), (J, 1), (_, 1)] -> ThreeOfAKind
    [(_, 2), (_, 1), (_, 1), (J, 1)] -> ThreeOfAKind
    [(_, 2), (_, 1), (_, 1), (_, 1)] -> OnePair

    [(J, 1), (_, 1), (_, 1), (_, 1), (_, 1)] -> OnePair
    [(_, 1), (J, 1), (_, 1), (_, 1), (_, 1)] -> OnePair
    [(_, 1), (_, 1), (J, 1), (_, 1), (_, 1)] -> OnePair
    [(_, 1), (_, 1), (_, 1), (J, 1), (_, 1)] -> OnePair
    [(_, 1), (_, 1), (_, 1), (_, 1), (J, 1)] -> OnePair
    [(_, 1), (_, 1), (_, 1), (_, 1), (_, 1)] -> HighCard


-- hand :: [Card] -> Hand
-- hand cs =
--   case reverse $ sortOn snd $ M.toList $ count cs of
--     [(c, _)] -> FiveOfAKind
--     [(c, 4), (d, 1)] -> FourOfAKind
--     [(c, 3), (d, 2)] -> FullHouse
--     [(c, 3), (d, 1), (e, 1)] -> ThreeOfAKind
--     [(c, 2), (d, 2), (e, 1)] -> TwoPair
--     [(c, 2), (d, 1), (e, 1), (f, 1)] -> OnePair
--     [(c, 1), (d, 1), (e, 1), (f, 1), (g, 1)] -> HighCard


count :: Ord a => [a] -> M.Map a Int
count xs = M.fromListWith (+) $ zip xs (repeat 1)

card 'A' = A
card 'K' = K
card 'Q' = Q
card 'J' = J
card 'T' = T
card '9' = N9
card '8' = N8
card '7' = N7
card '6' = N6
card '5' = N5
card '4' = N4
card '3' = N3
card '2' = N2
