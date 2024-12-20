import Data.Array (Ix(inRange))
import Control.Monad (foldM)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARTS

(part1, part2) = (part False, part True)

part canRemove =
    length .
    filter (\xs -> isSafe xs || isSafe (reverse xs)) .
    map (map (read @Int) . words)
    where
    isSafe = not . null . foldM takeItem (Nothing, canRemove)

takeItem (prev, canRemove) x =
    [(prev,   False)     | canRemove] ++
    [(Just x, canRemove) | canAccept prev x]

canAccept Nothing     _ = True
canAccept (Just prev) x = inRange (1, 3) (x - prev)
