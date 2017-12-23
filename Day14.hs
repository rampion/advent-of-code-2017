{-
--- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the
situation different, you might sit and watch it for a while, but today, you
just don't have that kind of time. It's soaking up valuable system resources
that are needed elsewhere, and so the only option is to help it finish its task
as soon as possible.

The disk in question consists of a 128x128 grid; each square of the grid is
either free or used. On this disk, the state of the grid is tracked by the bits
in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single row
in the grid; each hash contains 128 bits which correspond to individual grid
squares. Each bit of a hash indicates whether that square is free (0) or used
(1).

The hash inputs are a key string (your puzzle input), a dash, and a number from
0 to 127 corresponding to the row. For example, if your key string were
flqrgnkx, then the first row would be given by the bits of the knot hash of
flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so
on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal
digits; each of these digits correspond to 4 bits, for a total of 4 * 32 = 128
bits. To convert to bits, turn each hexadecimal digit to its equivalent binary
value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f
becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal
would begin with 10100000110000100000000101110000... in binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx appear
as follows, using # to denote used squares, and . to denote free ones:

##.#.#..-->
.#.#.#.#   
....#.#.   
#.#.##.#   
.##.#...   
##..#..#   
.#...#..   
##.#.##.-->
|      |   
V      V   

In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?

Your puzzle input is uugsqrei.
-}
{-# OPTIONS_GHC -Wall -Werror -Wextra #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Day10 (hash, Hash(..))
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Monad.State
import Control.Arrow (first)
import Numeric

main :: IO ()
main = return ()

puzzleInput :: String
puzzleInput = "uugsqrei"

exampleGrid, problemGrid :: Grid
exampleGrid = grid "flqrgnkx"
problemGrid = grid "uugsqrei"

-- X
-- >>> part1 "flqrgnkx"
-- 8108
-- >>> part1 "uugsqrei"
-- 8194
part1 :: String -> Int
part1 = V.sum . fmap popCount . grid

grid :: String -> Grid
grid str = V.fromList [ getHash . hash $ str ++ '-' : show n | n <- [0..127 :: Int] ]

{-
--- Part Two ---

Now, all the defragmenter needs to know is the number of regions. A region is a
group of used squares that are all adjacent, not including diagonals. Every
used square is in exactly one region: lone used squares form their own isolated
regions, while several adjacent squares all count as a single region.

In the example above, the following nine regions are visible, each marked with
a distinct digit:

11.2.3..-->
.1.2.3.4   
....5.6.   
7.8.55.9   
.88.5...   
88..5..8   
.8...8..   
88.8.88.-->
|      |   
V      V   

Of particular interest is the region marked 8; while it does not appear
contiguous in this small view, all of the squares marked 8 are connected when
considering the whole 128x128 grid. In total, in this example, 1242 regions are
present.

How many regions are present given your key string?
-}

type Grid = V.Vector Integer

showGrid :: Grid -> String
showGrid = V.foldr go "" where
  showLine = showIntAtBase 2 $ \case 
    0 -> '.'
    _ -> '#'
  go i = tail . showLine (i + n) . ('\n':)
  n = 2 ^ (128 :: Int)


-- |
-- >>> fst $ dfs exampleGrid (127,0) S.empty
-- [(127,0),(126,0),(126,1)]
-- >>> fst $ dfs exampleGrid (124,0) S.empty
-- [(124,0),(124,1)]
dfs :: Grid -> (Int,Int) -> S.Set (Int,Int) -> ([(Int,Int)], S.Set (Int,Int))
dfs g = \p -> go [p] where
  go [] seen = ([], seen)
  go (p@(x,y):ps) seen 
    | testBit (g V.! y) x && not (S.member p seen) = 
          first (p:) $ (go . concat)
            [ [ (x - 1, y) | x - 1 >= 0 ] 
            , [ (x + 1, y) | x + 1 < 128]
            , [ (x, y - 1) | y - 1 >= 0 ]
            , [ (x, y + 1) | y + 1 < 128]
            , ps
            ] (S.insert p seen)
    | otherwise = go ps seen

-- |
-- >>> length $ regions exampleGrid
-- 1242
-- >>> length $ regions problemGrid
-- 1141
regions :: Grid -> [[(Int,Int)]]        
regions g = filter (not . null) . flip evalState S.empty $ 
  forM [(x,y) | x <- [0..127], y <- [0..127]] $ 
    state . dfs g
