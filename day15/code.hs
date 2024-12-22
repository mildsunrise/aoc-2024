{-# LANGUAGE ViewPatterns #-}
import Data.Array (array, (//), (!), assocs, bounds)
import Control.Monad (guard)
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

mapFind1 grid x =
    (\[(p, _)] -> p) $
    filter ((== x) . snd) $
    assocs grid

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]


-- PARSING

parseInput f (splitOn [""] ->
    [mapToArray . map f -> grid, concat -> moves]) =
        (grid, map (fromJust . (`elemIndex` "^>v<")) moves)


-- PARTS

pushTile h '.' = []
pushTile h 'O' = [(0, 0)]
pushTile h t | elem t "[]" && odd h = [(0, 0)]
pushTile h '[' = [(0, 0), (1, 0)]
pushTile h ']' = [(-1, 0), (0, 0)]

tryMove grid h = fromMaybe grid $ do
    boxes <- pushedTiles (push pos)
    pure $ grid // displace (pos:boxes)
  where
    pos = mapFind1 grid '@'
    push = pzip (+) (headings !! h)
    pushedTiles pos = do
        guard (grid ! pos /= '#')
        let dpos' = pushTile h (grid ! pos)
        let pos' = map (pzip (+) pos) dpos'
        deps <- mapM (pushedTiles . push) pos'
        pure $ pos' ++ concat deps
    displace tiles =
        map (, '.') tiles ++
        map (\pos -> (push pos, grid ! pos)) tiles

coordinatesSum =
    sum .
    map (\((x, y), _) -> 100 * y + x) .
    filter ((`elem` ['O', '[']) . snd) .
    assocs

part f (parseInput f -> (grid, moves)) =
    coordinatesSum $
    foldl tryMove grid moves

widen '#' = "##"
widen 'O' = "[]"
widen '.' = ".."
widen '@' = "@."

(part1, part2) = (part id, part (concatMap widen))
