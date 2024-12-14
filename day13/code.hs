{-# LANGUAGE ViewPatterns #-}
import Data.List.Split (splitOn)
import Data.List (uncons, transpose, find)
import Data.Ratio (Rational, numerator, denominator)
import Control.Monad (guard)
import Data.Maybe (fromJust, mapMaybe)
import Control.Arrow (Arrow(second))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- ALGORITHMS

solve k xs =
  fmap (map (\[a] -> a) . fst) $
  iterate (>>= solveStep) (Just ([], xs)) !! k

solveStep (as, xs) = do
  (bs, xs) <- pure $ span ((== 0) . head) xs
  (k:p, xs) <- uncons xs
  p <- pure $ map (* (1/k)) p
  let f (k:r) = zipWith (-) r $ map (* k) p
  pure (map f as ++ [p], map f (bs ++ xs))

ratioAsInt :: Rational -> Maybe Integer
ratioAsInt q =
  numerator q <$ guard (denominator q == 1)


-- PARSING

parseInput = map arcade . splitOn [""]
  where
    component c (splitOn [c] ->
      [axis, read @Int -> n]) = (axis, n)

    vector c (map (component c) . splitOn ", " ->
      [("X", x), ("Y", y)]) = [x, y]

    line c (splitOn ": " ->
      [name, vector c -> v]) = (name, v)

    arcade
      [ line '+' -> ("Button A", ba)
      , line '+' -> ("Button B", bb)
      , line '=' -> ("Prize", p)
      ] = ((ba, bb), p)


-- PARTS

solveArcade ((ba, bb), p) =
  mapM (find (>= 0) . ratioAsInt) $
  fromJust $
  solve 2 $
  map (map fromIntegral) $
  transpose [ba, bb, p]

part f =
  sum .
  map (sum . zipWith (*) [3, 1]) .
  mapMaybe (solveArcade . f) .
  parseInput

part1 = part id
part2 = part (second $ map (+ 10000000000000))
