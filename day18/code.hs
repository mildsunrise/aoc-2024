{-# LANGUAGE ViewPatterns #-}
import Data.Maybe (fromJust, isNothing)
import Data.List (findIndex)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Control.Arrow (Arrow(first))
import Data.Array (accumArray, (!?))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]

bfsStep edges (visited, nodes) =
    (Set.union nodes visited,) $
    Set.fromList $
    concatMap edges $
    Set.toList (nodes Set.\\ visited)

bfs edges start =
    takeWhile (not . null) $
    map (Set.toList . snd) $
    iterate (bfsStep edges)
    (Set.empty, Set.singleton start)

bisect pred lo hi
    | mid == lo = hi
    | pred mid = bisect pred lo mid
    | otherwise = bisect pred mid hi
    where mid = (lo + hi) `div` 2


-- PARSING

parseVector (map (read @Int) . splitOn "," -> [x, y]) = (x, y)


-- PARTS

edges grid pos =
    filter ((== Just False) . (grid !?)) $
    map (pzip (+) pos) headings

part n =
    findIndex (elem (70, 70)) .
    (\grid -> bfs (edges grid) (0, 0)) .
    accumArray (||) False ((0, 0), (70, 70)) .
    take n .
    map ((,True) . parseVector)

part1 = fromJust . part 1024

part2 xs =
    (xs !!) $
    bisect (isNothing . (`part` xs)) 0 (length xs) - 1
