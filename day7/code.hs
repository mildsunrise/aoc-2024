{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
import Data.List.Split (splitOn)
import Data.Foldable1 (foldlM1)
import Data.List.NonEmpty (fromList)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

parseEquation (splitOn ":" -> [a, b]) =
    (read @Int a, fromList $ map read $ words b)

isValid ops (r, xs) = elem r $ foldlM1 step xs
  where
    step a b = filter (<= r) $ map (\f -> f a b) ops

part ops =
    sum .
    map fst .
    filter (isValid ops) .
    map parseEquation

concatNum x y = read (show x ++ show y)

part1 = part [(+), (*)]
part2 = part [(+), (*), concatNum]
