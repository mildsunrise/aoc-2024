{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns, TypeApplications #-}

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PART 1

part1 x = 0


-- PART 2

part2 x = 0
