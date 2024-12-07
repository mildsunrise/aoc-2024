{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.Array (array, assocs, (!?), (//))
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (unfoldr, inits)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- GENERIC ALGORITHMS

pzip f (a, b) (c, d) = (f a c, f b d)

iterateM f = unfoldr (fmap (\a -> (a, a)) . f)

mapToArray lines = array ((0, 0), bound) entries
  where
    bound = (length (head lines) - 1, length lines - 1)
    entries = concat $ zipWith (\y -> zip (map (,y) [0..])) [0..] lines

findCycle :: Ord a => [a] -> Maybe (Map.Map a Int, Int)
findCycle = (`helper` Map.empty) . zip [0..]
  where
  helper [] m = Nothing
  helper ((i,x):xs) m =
    either (Just . (m,)) (helper xs) $
    Map.alterF (maybe (Right $ Just i) Left) x m


-- PART 1

headings = [(0, -1), (1, 0), (0, 1), (-1, 0)]

findGuard =
    (\[(p, h)] -> (p, 0)) .
    filter ((== '^') . snd) .
    assocs

step grid (p, h) = do
    let p' = pzip (+) p (headings !! h)
    cell <- grid !? p'
    return $ case cell of
        '#' -> (p, (h + 1) `mod` 4)
        _   -> (p', h)

part1 (mapToArray -> grid) =
    length $
    nubOrdOn fst $
    iterateM (step grid) (findGuard grid)


-- PART 2

placeObstruction grid ((p, h), prev) = isJust $ do
    let p' = pzip (+) p (headings !! h)
    guard (grid !? p' == Just '.')
    guard (notElem p' $ map fst prev)
    let grid' = grid // [(p', '#')]
    findCycle (iterateM (step grid') (p, h))

part2 (mapToArray -> grid) =
    length $
    filter (placeObstruction grid) $
    (\xs -> zip xs (inits xs)) $
    iterateM (step grid) (findGuard grid)
