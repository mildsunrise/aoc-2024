import qualified Data.Map as Map

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

blink x
    | x == 0 = [1]
    | even xLen = map read [a, b]
    | otherwise = [x * 2024]
  where
    xStr = show x
    xLen = length xStr
    (a, b) = splitAt (xLen `div` 2) xStr

blinkMany =
    Map.fromListWith (+) .
    concatMap (\(k, v) -> map (,v) (blink k)) .
    Map.toList

part n =
    sum .
    Map.elems .
    (!! n) .
    iterate blinkMany .
    Map.fromListWith (+) .
    map ((,1) . read @Int) .
    (\[l] -> words l)

(part1, part2) = (part 25, part 75)
