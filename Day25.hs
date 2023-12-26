import Data.Array qualified as A
import Data.Graph qualified as G
import Data.Map qualified as M
import Data.Set qualified as S

test = pt2 <$> readFile "test25.txt"

main = pt2 <$> readFile "input25.txt"

pt2 input = graph
  where
    withoutMissing = M.fromList $ map parse $ lines input
    dict = M.union withoutMissing (M.fromList [(k, []) | k <- concat (M.elems withoutMissing)])
    (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges [(k, k, vs) | (k, vs) <- M.toList dict]

parse line = let w : ws = words line in (init w, ws)

mincut graph w
