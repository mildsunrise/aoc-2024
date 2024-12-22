import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(Empty, (:<|), (:|>)), (><))

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- COMMON

parseBlocks =
    filter ((/= 0) . snd) .
    zip (concatMap blockLabels [0..]) .
    map (read @Int . (: [])) .
    (\[a] -> a)
    where
    blockLabels f = [Just f, Nothing]

toBlocks = concatMap (\(l, n) -> replicate n l)

checksum =
    sum .
    catMaybes .
    zipWith (\i -> fmap (* i)) [0..]

part f = checksum . f . parseBlocks


-- PART 1

compact Empty = []
compact (xs :|> Nothing) = compact xs
compact (Just f :<| xs) = f : compact xs
compact (Nothing :<| (xs :|> Just f)) = f : compact xs

part1 = part $
    map Just .
    compact .
    Seq.fromList .
    toBlocks


-- PART 2

hasSpace m (Nothing, n) = n >= m
hasSpace _ _ = False

defragment Empty = Empty
defragment (xs :|> f@(fn, 0)) = defragment xs
defragment (xs :|> f@(fn, m)) = defragment xs' :|> f'
  where
    (xs', f') = case (Seq.breakl (hasSpace m) xs, fn) of
        ((xs, (Nothing, n) :<| ys), Just _) ->
            ((xs :|> f :|> (Nothing, n - m)) >< ys, (Nothing, m))
        _ -> (xs, f)

part2 = part $
    toBlocks .
    toList .
    defragment .
    Seq.fromList
