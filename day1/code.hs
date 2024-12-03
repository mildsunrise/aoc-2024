main = getContents >>= (print . parts . lines)

parts x = (part1 x, part2 x)

part1 x = 0

part2 x = 0
