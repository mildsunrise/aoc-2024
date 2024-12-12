{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as Map
import Data.Array (array, assocs, (!), bounds, Ix (inRange))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines


-- PARTS

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]

edges grid p =
    filter (\p' -> grid ! p' == succ (grid ! p)) $
    filter (inRange (bounds grid)) $
    map (pzip (+) p) headings

trailheads =
    map fst .
    filter ((== '0') . snd) .
    assocs

bfsStep grid =
    Map.toList .
    Map.fromListWith (+) .
    concatMap (\(p, n) -> map (,n) (edges grid p))

bfs grid start =
    iterate (bfsStep grid) [(start, 1)] !! 9

part (mapToArray -> grid) =
    concatMap (bfs grid) (trailheads grid)

part1 = length . part
part2 = sum . map snd . part
