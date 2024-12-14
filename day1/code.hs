{-# LANGUAGE ViewPatterns #-}
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sort)
import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

parseLine (map (read @Int) . words -> [a, b]) = (a, b)

part pre join post =
    sum .
    post .
    uncurry join .
    bimap pre pre .
    unzip .
    map parseLine

part1 = part sort (zipWith (-)) (map abs)

part2 = part
    (Map.fromListWith (+) . map (,1))
    (Map.intersectionWith (*))
    (map (uncurry (*)) . Map.toList)
