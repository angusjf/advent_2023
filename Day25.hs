import qualified Data.Map as M
import qualified Data.Set as S

test = pt2 <$> readFile "test25.txt"
main = pt2 <$> readFile "input25.txt"

pt2 input = let xs = map parse $ lines input in solve (M.fromList xs) (S.fromList $ map fst xs)

parse line = let w:ws = words line in (init w, ws)

solve dict xs =
  let (next, more) = S.deleteFindMin xs in solve' dict next more

solve' dict next more =
  let result = map (\n -> solve' dict n more) (dict M.! next)
   in
