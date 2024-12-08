My (cursed) solutions for AoC 2024.

They are written in a variety of languages (depending on what I felt like each day).
But mostly in Haskell.

Semi-useful info is given for some of the languages below:

## Haskell solutions

Other than the standard libraries, the only dependencies are:
- `split`: because Haskell can apparently not agree on providing a split function in the stdlib, and it's kinda useful for parsing
- `fgl`: a graph library, because AoC seems to be a **very big** fan of graphs and I'm tired of re-implementing the same algorithms year after year.

Proper packaging? cabal? What's that?

The solutions read the input from stdin and `print` a tuple with two ints (for both parts). Run like this:
``` bash
runghc day2/code.hs < day2/input.txt
```

## Uiua solutions

They always begin by loading the input `&fras "input.txt"`.
After that there's usually a line which applies very basic parsing to put the long string into a matrix.
This is almost always the standard `⊜(⊜⋕⊸≠@ )⊸≠@\n` (for space-separated number matrices) or just `⊜∘ ⊸≠@\n` (for 2D maps).

The solution then defines functions, and typically calls the functions for each part with `⊃`, leaving part 1 result at the top of the stack, and part 2 beneath it.
