{-# LANGUAGE ViewPatterns #-}
import Data.Array (array, assocs, bounds, (!), Ix (range))
import Data.Graph.Inductive (Gr, mkMapGraph, mkNode_, lab, spTree, LPath (LP))
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Containers.ListUtils (nubOrd)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]


-- PARTS

findCell c =
    (\[(p, h)] -> p) .
    filter ((== c) . snd) .
    assocs

nodes =
    concatMap (\(p,_) -> map (p,) [0..3]) .
    filter ((/= '#') . snd) .
    assocs

edges grid n@(pos, h) = fwd ++ map turn [1, -1]
  where
    pos' = pzip (+) pos (headings !! h)
    fwd = [(n, (pos', h), 1) | grid ! pos' /= '#']
    turn dh = (n, (pos, (h + dh) `mod` 4), 1000)

toGraph grid = mkMapGraph @_ @Gr
    (nodes grid)
    (concatMap (edges grid) (nodes grid))

part (mapToArray -> grid) =
    filter ((== end) . head . snd) $
    map (\(LP path@((_,d):_)) -> (d, map nodePos path)) $
    spTree (fst $ mkNode_ nm (start, 1)) graph
  where
    [start, end] = map (`findCell` grid) ['S', 'E']
    (graph, nm) = toGraph grid
    nodePos (lab graph -> Just (pos, _), _) = pos

spotsToSit paths@(first:_) =
    length $
    nubOrd $
    concatMap snd $
    takeWhile (on (==) fst first) paths

part1 = fst . head . part
part2 = spotsToSit . part
