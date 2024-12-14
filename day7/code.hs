{-# LANGUAGE ViewPatterns #-}
import Data.List.Split (splitOn)
import Data.Foldable (foldrM)
import Control.Monad (guard)
import Data.List.Extra (stripSuffix)
import Data.Maybe (mapMaybe)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

parseEquation (splitOn ":" -> [a, b]) =
    (read @Int a, map read $ words b)

isValid ops (r, x : xs) = elem x $ foldrM step r xs
  where
    step a b = mapMaybe (\f -> f a b) ops

part ops =
    sum .
    map fst .
    filter (isValid ops) .
    map parseEquation

addOp y r = x <$ guard (x > 0) where x = r - y
mulOp y r = x <$ guard (x*y == r) where x = div r y
catOp y r = read . ('0' :) <$> stripSuffix (show y) (show r)

part1 = part [addOp, mulOp]
part2 = part [addOp, mulOp, catOp]
