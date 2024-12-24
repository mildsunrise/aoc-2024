{-# LANGUAGE ViewPatterns #-}
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List (sortOn, sort, intercalate)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

subsetsOf 0 _ = [[]]
subsetsOf _ [] = []
subsetsOf n (x:xs) =
    map (x:) (subsetsOf (n-1) xs) ++ subsetsOf n xs

cliques hasEdge include [] [] = [include]
cliques hasEdge include exclude [] = []
cliques hasEdge include exclude (node:nodes) =
    cliques hasEdge (node:include) (f exclude) (f nodes) ++
    cliques hasEdge include (node:exclude) nodes
    where f = filter (hasEdge node)


-- PARSING

parseEdge (splitOn "-" -> [a, b]) = [(a, b), (b, a)]

parseGraph = Set.fromList . concatMap parseEdge


-- PARTS

part (parseGraph -> graph) =
    cliques (curry (`Set.member` graph)) [] [] $
    Set.toList $ Set.map fst graph

part1 =
    Set.size .
    Set.fromList .
    map Set.fromList .
    filter (any ((== 't') . head)) .
    concatMap (subsetsOf 3) .
    part

part2 =
    intercalate "," .
    sort .
    last .
    sortOn length .
    part
