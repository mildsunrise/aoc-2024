{-# LANGUAGE ViewPatterns #-}
import Data.List (inits, tails)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseInput (splitOn [""] ->
    [[splitOn ", " -> towels], designs]) =
        (Set.fromList towels, designs)


-- PARTS

arrangements towels design = head xs
  where
    xs = zipWith f (tails xs) (tails design)

    f _  [] = 1
    f xs ys =
        sum $
        map fst $
        filter ((`Set.member` towels) . snd) $
        zip xs (inits ys)

part (parseInput -> (towels, designs)) =
    map (arrangements towels) designs

part1 = length . filter (/= 0) . part
part2 = sum . part
