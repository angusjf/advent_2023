test = pt2 <$> readFile "test09.txt"
main = pt2 <$> readFile "input09.txt"

pt2 =
  sum .
  map (solve . map read . words) .
  lines

solve = foldr1 (-) . map head . triangulate

triangulate = takeWhile (any (/= 0)) . iterate diffs

diffs xs = zipWith (-) (tail xs) xs
