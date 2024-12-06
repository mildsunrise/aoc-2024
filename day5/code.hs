{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Graph.Inductive
    ( Graph(mkGraph), mkUGraph, nfilter, topsort )
import Data.Graph.Inductive.PatriciaTree (UGr)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseInput (splitOn [""] -> [a, b]) =
    (map parseOrderRule a, map parseUpdate b)

parseOrderRule (map (read @Int) . splitOn "|" -> [a, b]) = (a, b)

parseUpdate = map (read @Int) . splitOn ","


-- PARTS

graphFromEdges :: [(Int, Int)] -> UGr
graphFromEdges es =
    mkUGraph (concatMap (\(a,b) -> [a,b]) es) es

middle xs = xs !! (length xs `div` 2)

part f (parseInput -> (graphFromEdges -> rules, updates)) =
    sum $
    map (middle . snd) $
    filter (uncurry f) $
    map (\pages -> (pages, sortUpdate rules pages)) updates

sortUpdate rules pages =
    topsort $
    nfilter (`Set.member` Set.fromList pages) rules

(part1, part2) = (part (==), part (/=))
