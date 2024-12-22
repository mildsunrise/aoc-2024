{-# LANGUAGE ViewPatterns #-}
import Data.Array (array, (!?), assocs, bounds, Ix (index, range))
import qualified Data.Graph.Inductive as Gr
import Data.Function (on)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]


-- PART 1

edges grid (i, (p, v)) =
    map ((i,,()) . index (bounds grid)) .
    filter ((== Just v) . (grid !?)) $
    map (pzip (+) p) headings

toGraph grid = Gr.mkGraph @Gr.Gr
    (zip [0..] $ range $ bounds grid)
    (concatMap (edges grid) $ zip [0..] $ assocs grid)

part f (mapToArray -> grid) =
    sum $
    map (f grid . (`Gr.subgraph` toGraph grid)) $
    Gr.components (toGraph grid)

perimeter region = Gr.order region * 4 - Gr.size region

part1 = part $ \_ region -> Gr.order region * perimeter region


-- PART 2

isCorner f = f 1 0 == f 0 1 && not (f 1 0 && f 1 1)

countCorners grid pos =
    sum [1 | sx <- [1,-1], sy <- [1,-1], isCorner (f (sx, sy))]
  where
    f k = curry (on (==) (grid !?) pos . pzip (+) pos . pzip (*) k)

sides grid region =
    sum $
    map (countCorners grid . snd) $
    Gr.labNodes region

part2 = part $ \grid region -> Gr.order region * sides grid region
