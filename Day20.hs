import Control.Monad.State
import Data.List
import Data.Map qualified as M

test = pt2 <$> readFile "test20.txt"

test2 = pt2 <$> readFile "test20_2.txt"

main = pt2 <$> readFile "input20.txt"

pt2 input = send dict Low "" "broadcaster"
  where
    dict = M.fromList . map parse . lines $ input

data S = High | Low deriving (Show, Eq)

data T = Broadcaster | FlipFlop Bool | Conjunction (M.Map String S) (M.Map String S) deriving (Show)

parse input = (id, (t, map init (init xs) ++ [last xs]))
  where
    a : "->" : xs = words input
    (t, id) = case a of
      "broadcaster" -> (Broadcaster, "broadcaster")
      '&' : id -> (Conjunction M.empty M.empty, id)
      '%' : id -> (FlipFlop False, id)

send dict signal from id = M.unions $ map (send dict' signal' id) xs
  where
    (t, xs) = dict M.! id
    (signal', dict') = case t of
      Broadcaster -> (signal, dict)
      -- Flip-flop modules (prefix %) are either on or off; they are initially off.
      -- If a flip-flop module receives a high pulse, it is ignored and nothing happens.
      -- However, if a flip-flop module receives a low pulse, it flips between on and off.
      -- If it was off, it turns on and sends a high pulse.
      -- If it was on, it turns off and sends a low pulse.
      FlipFlop state ->
        case signal of
          High -> (signal, dict')
          Low ->
            ( if state then High else Low,
              M.insert id (FlipFlop (not state), xs) dict
            )
      -- Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules;
      -- they initially default to remembering a low pulse for each input.
      -- When a pulse is received, the conjunction module first updates its memory for that input.
      -- Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.
      Conjunction prevs building ->
        let building' = M.insert from signal prevs
         in ( if all' (== High) (M.elems prevs) then Low else High,
              M.insert id (Conjunction prevs building', xs) dict
            )

all' _ [] = False
all' f xs = all f xs
