{-# LANGUAGE ViewPatterns #-}
import Data.Array (array, assocs, (!))
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow (Arrow(first, second))
import Data.List (mapAccumL)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]

dijkstra edges start = step Set.empty
    (Set.singleton (0, start))
    (Map.singleton start (0, []))
  where
    step _ queue dists | Set.null queue = dists
    step visited (Set.deleteFindMin -> ((d, n), queue)) dists =
        case Set.alterF (, True) n visited of
            (True, _) -> step visited queue dists
            (False, visited) -> step visited queue' dists'
      where
        nodes = map (first (+ d)) (edges n)
        queue' = foldl (flip Set.insert) queue nodes
        dists' = foldl (flip (updateDist n)) dists nodes

updateDist n (d', n') = Map.alter merge n'
  where
    merge (Just (d, pred)) | d <= d' =
        Just (d, [n | d == d'] ++ pred)
    merge _ = Just (d', [n])

dfs_ edges = snd . dfs edges Set.empty

dfs edges visited node =
    case Set.alterF (, True) node visited of
        (True, _) -> (visited, [])
        (False, visited) ->
            second ((node :) . concat) $
            mapAccumL (dfs edges) visited (edges node)

dfs__ edges node =
    node : concatMap (dfs__ edges) (edges node)


-- PARTS

data Node = Cell (Int, Int) Int | Won
    deriving (Eq, Ord, Show)

findStart =
    (\[(pos, _)] -> Cell pos 1) .
    filter ((== 'S') . snd) .
    assocs

edges grid Won = []
edges grid (Cell pos h) =
    [(0, Won) | grid ! pos == 'E'] ++
    [(1, Cell pos' h) | grid ! pos' /= '#'] ++
    map (\h -> (1000, Cell pos (h `mod` 4))) [h - 1, h + 1]
  where
    pos' = pzip (+) pos (headings !! h)

part (mapToArray -> grid) =
    dijkstra (edges grid) (findStart grid)

spotsToSit dists =
    length $ nubOrd $
    map (\(Cell pos _) -> pos) $
    tail $ dfs_ (fmap snd dists Map.!) Won

part1 = fst . (Map.! Won) . part
part2 = spotsToSit . part
