{-# LANGUAGE ViewPatterns #-}
import Data.Bits (Bits(shiftR, xor, (.&.)))
import Data.Maybe (catMaybes)
import Data.List (unfoldr, intercalate)
import Data.Array (listArray, (!), (!?), (//))
import Data.List.Split (splitOn)
import Control.Monad (guard)

main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)


-- PARSING

parseRegister (splitOn ": " -> [_, read @Int -> x]) = x

parseCode (splitOn ": " -> ["Program",
    map (read @Int) . splitOn "," -> code]) =
        listArray (0, length code - 1) code

parseInput (splitOn [""] -> [
    map parseRegister -> [a, b, c],
    [parseCode -> code]]) =
        (listArray (0, 3) [a, b, c, 0], code)


-- PART 1

getCombo regs x
    | x < 4 = x
    | x < 7 = regs ! (x - 4)

comboOp op regs x = op regs (getCombo regs x)
storeOp dest op regs x = (Nothing, regs // [(dest, op regs x)])

xdv dest = storeOp dest $ comboOp $ \regs x -> (regs ! 0) `shiftR` x
bxl = storeOp 1 $ \regs x -> (regs ! 1) `xor` x
bxc = storeOp 1 $ \regs _ -> (regs ! 1) `xor` (regs ! 2)
bst = storeOp 1 $ comboOp $ \regs x -> x .&. 7
jnz = storeOp 3 $ \regs x -> if (regs ! 0) /= 0 then x else regs ! 3
out = comboOp $ \regs x -> (Just (x .&. 7), regs)

ops = listArray (0, 7) [xdv 0, bxl, bst, jnz, bxc, out, xdv 1, xdv 2]

execOp code regs = do
  opcode <- code !? (regs ! 3)
  x <- code !? succ (regs ! 3)
  let regs' = regs // [(3, (regs ! 3) + 2)]
  pure $ (ops ! opcode) regs' x

part1 (parseInput -> (regs, code)) =
  intercalate "," $
  map show $
  catMaybes $
  unfoldr (execOp code) regs


-- PART 2

part2 x = 0
