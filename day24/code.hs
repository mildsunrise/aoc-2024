{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List (sort, intercalate, mapAccumL, tails, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Bits (shift)
import Text.Printf (printf)
import Control.Monad (guard)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseInitial (splitOn ": " -> [name, value]) = (name, value /= "0")

parseGate (words -> [a, op, b, "->", d]) = (d, (op, a, b))

parseInput (splitOn [""] ->
    [map parseInitial -> initial, map parseGate -> gates]) =
        (Map.fromList initial, Map.fromList gates)


-- PART 1

eval op initial gates = xs
  where
    xs = Map.union initial (evalGate <$> gates)
    evalGate (op -> f, a, b) = f (xs Map.! a) (xs Map.! b)

op "AND" = (&&)
op "OR" = (||)
op "XOR" = (/=)

part1 (parseInput -> (initial, gates)) =
    sum $
    map (\('z':n, v) -> fromEnum v `shift` read n) $
    filter ((== 'z') . head . fst) $
    Map.toList (eval op initial gates)


-- PART 2

xor a b = Set.union (a Set.\\ b) (b Set.\\ a)

xorList :: Ord a => [a] -> Set.Set a
xorList =
    Map.keysSet .
    Map.filter odd .
    Map.fromListWith (+) .
    map (,1)

andOp a b = xorList (Set.union <$> Set.toList a <*> Set.toList b)
orOp a b = xor (xor a b) (andOp a b)

op' "AND" = andOp
op' "OR" = orOp
op' "XOR" = xor

inputs = Map.fromList $ flip concatMap [0..44] $ \(n :: Int) -> let
    name (c :: Char) = c : printf "%02d" n
    term = Set.singleton . Set.singleton . name
    x = (name 'x', term 'x')
    y = (name 'y', term 'x' `xor` term 't')
    in [x, y]

adder carry (n :: Int) = let
    name c = c : printf "%02d" n
    term = Set.singleton . Set.singleton . name
    carry' = term 'x' `xor` (term 't' `andOp` (term 'x' `xor` carry))
    in (carry', (name 'z', term 't' `xor` carry))

outputs = Map.fromList $ snd $ mapAccumL adder Set.empty [0..44]

discriminant = Map.intersectionWith xor outputs . eval op' inputs

dependencies gates = xs
  where
    xs = Map.mapWithKey eval gates
    deps k = fromMaybe Set.empty (xs Map.!? k)
    eval k (_, a, b) = Set.insert k $ Set.union (deps a) (deps b)

swapGates gates (a, b) =
    Map.insert a (gates Map.! b) $
    Map.insert b (gates Map.! a) gates

canSwap deps (a, b) =
    Set.notMember b (deps Map.! a) &&
    Set.notMember a (deps Map.! b)

validSwaps deps = filter (canSwap deps)
    [(a, b) | a:bs <- init (tails (Map.keys deps)), b <- bs]

findNextSwap gates =
    (bestSwap, swapGates gates bestSwap) <$ guard (not (null incorrect))
  where
    deps = dependencies gates
    (correct, incorrect) = Map.partition null $ discriminant gates
    incorrectOut = head $ Map.keys incorrect
    noTouch = Set.unions $ map (deps Map.!) $ Map.keys correct
    candidates = (deps Map.! incorrectOut) Set.\\ noTouch
    isCandidate xs = any (`Set.member` candidates) xs && all (`Set.notMember` noTouch) xs
    swapScore = length . (Map.! incorrectOut) . discriminant . swapGates gates
    candidateSwaps = filter (\(a, b) -> isCandidate [a, b]) $ validSwaps deps
    bestSwap = snd $ minimum $ map (\x -> (swapScore x, x)) candidateSwaps

part2 (parseInput -> (_, gates)) = unfoldr findNextSwap gates
