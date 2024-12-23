import Data.Bits (shift, (.&.), xor)
import qualified Data.Map as Map
import Data.List (tails)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PART 1

update x = foldl op x [6, -5, 11]
  where
    op x s = (x `xor` shift x s) .&. (shift 1 24 - 1)

updates = take 2001 . iterate update

part1 = sum . map (last . updates . read @Int)


-- PART 2

prices = map (`mod` 10) . updates

diffSeqs xs = Map.fromListWith (const id) ys
  where
    ds = zipWith (-) (tail xs) xs
    seqs = map (take 4) $ tails ds
    ys = zip seqs (drop 4 xs)

part2 =
    maximum .
    Map.elems .
    foldl (Map.unionWith (+)) Map.empty .
    map (diffSeqs . prices . read @Int)
