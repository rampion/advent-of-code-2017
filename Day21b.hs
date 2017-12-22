{-
--- Day 21: Fractal Art ---

You find a program trying to generate some art. It uses a strange process that
involves repeatedly enhancing the detail of an image through a set of rules.

The image consists of a two-dimensional square grid of pixels that are either on
(#) or off (.). The program always begins with this pattern:

.#.
..#
###

Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have
a size of 3.

Then, the program repeats the following process:

    If the size is evenly divisible by 2, break the pixels up into 2x2 squares,
    and convert each 2x2 square into a 3x3 square by following the corresponding
    enhancement rule.

    Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3
    squares, and convert each 3x3 square into a 4x4 square by following the
    corresponding enhancement rule.

Because each square of pixels is replaced by a larger one, the image gains
pixels and so its size increases.

The artist's book of enhancement rules is nearby (your puzzle input); however,
it seems to be missing rules. The artist explains that sometimes, one must
rotate or flip the input pattern to find a match. (Never rotate or flip the
output pattern, though.) Each pattern is written concisely: rows are listed as
single units, ordered top-down, and separated by slashes. For example, the
following rules correspond to the adjacent patterns:

../.#  =  ..
          .#

                .#.
.#./..#/###  =  ..#
                ###

                        #..#
#..#/..../#..#/.##.  =  ....
                        #..#
                        .##.

When searching for a rule to use, rotate and flip the pattern as necessary. For
example, all of the following patterns match the same rule:

.#.   .#.   #..   ###
..#   #..   #.#   ..#
###   ###   ##.   .#.

Suppose the book contained the following two rules:

../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#

As before, the program begins with this pattern:

.#.
..#
###

The size of the grid (3) is not divisible by 2, but it is divisible by 3. It
divides evenly into a single square; the square matches the second rule, which
produces:

#..#
....
....
#..#

The size of this enhanced grid (4) is evenly divisible by 2, so that rule is
used. It divides evenly into four squares:

#.|.#
..|..
--+--
..|..
#.|.#

Each of these squares matches the same rule (../.# => ##./#../...), three of
which require some flipping and rotation to line up with the rule. The output
for the rule is the same in all four cases:

##.|##.
#..|#..
...|...
---+---
##.|##.
#..|#..
...|...

Finally, the squares are joined into a new grid:

##.##.
#..#..
......
##.##.
#..#..
......

Thus, after 2 iterations, the grid contains 12 pixels that are on.

How many pixels stay on after 5 iterations?
-}
module Main where
import Control.Monad
import Control.Monad.ST
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V'

main :: IO ()
main = do
  Right rm <- parseFromFile rules "Day21.input"
  print $ part2 rm

type Rules = M.Map String String

rules :: Parsec String () Rules
rules = M.fromList <$> (rule `endBy` newline)

rule :: Parsec String () (String, String)
rule = (,) <$> pattern <*> (string " => " *> pattern)

pattern :: Parsec String () String
pattern = many1 $ oneOf ".#/"

part1 :: Rules ->  Int
part1 rm = numSet 
  . step rm . step rm . step rm . step rm . step rm 
  $ initial

{-
--- Part Two ---

How many pixels stay on after 18 iterations?
-}
part2 :: Rules ->  Int
part2 rm = numSet 
  .  step rm . step rm . step rm . step rm . step rm . step rm 
  .  step rm . step rm . step rm . step rm . step rm . step rm 
  .  step rm . step rm . step rm . step rm . step rm . step rm 
  $ initial


type Grid = V.Vector (V.Vector Char)

initial :: Grid
initial = V.fromList . map V.fromList $ 
  [ ".#."
  , "..#"
  , "###"
  ]

-- |
-- >>> numSet initial
-- 5
numSet :: Grid -> Int
numSet g = length $ do
  let n = V.length g
  i <- [0..n-1]
  j <- [0..n-1]
  guard $ g V.! i V.! j == '#'

example :: Rules
example = M.fromList
  [ ("../.#", "##./#../...")
  , (".#./..#/###", "#..#/..../..../#..#")
  ]

-- |
-- >>> putStrLn $ showGrid initial
-- 	.#.
-- 	..#
-- 	###
showGrid :: Grid -> String
showGrid = L.intercalate ("\n") . map (('\t':) . V.toList) . V.toList

-- |
-- >>> next = step example initial
-- >>> putStrLn $ showGrid next
-- 	#..#
-- 	....
-- 	....
-- 	#..#
-- >>> putStrLn . showGrid $ step example next
-- 	##.##.
-- 	#..#..
-- 	......
-- 	##.##.
-- 	#..#..
-- 	......
step :: Rules -> Grid -> Grid
step rm g = runST $ do
  let n = V.length g
  let t | even n = 2
        | otherwise = 3
  let q = n `div` t
  let m = q * (t + 1)
  g' <- V.replicateM m (V'.new m)
  forM_ [0..q-1] $ \i ->
    forM_ [0..q-1] $ \j -> do
      let ys = [i*t + k | k <- [0..t-1]]
      let xs = [j*t + k | k <- [0..t-1]]
      let ks = L.intercalate "/" <$>
            [ [ [ g V.! y V.! x | x <- xs ] | y <- ys ]
            , [ [ g V.! y V.! x | x <- xs ] | y <- reverse ys ]
            , [ [ g V.! y V.! x | x <- reverse xs ] | y <- ys ]
            , [ [ g V.! y V.! x | x <- reverse xs ] | y <- reverse ys ]
            , [ [ g V.! y V.! x | y <- ys ] | x <- xs ]
            , [ [ g V.! y V.! x | y <- ys ] | x <- reverse xs ]
            , [ [ g V.! y V.! x | y <- reverse ys ] | x <- xs ]
            , [ [ g V.! y V.! x | y <- reverse ys ] | x <- reverse xs ]
            ]
      let r = head . catMaybes $ map (flip M.lookup rm) ks

      let rows = breaks (=='/') r
      let ys' = [i*(t + 1) + k | k <- [0..t]]
      let xs' = [j*(t + 1) + k | k <- [0..t]]
      forM_ (zip ys' rows) $ \(y',row) ->
        forM_ (zip xs' row) $ \(x',c) ->
          V'.write (g' V.! y') x' c
  V.mapM V.freeze g'

-- |
-- >>> breaks (=='/') "123/456/789"
-- ["123","456","789"]
breaks :: (a -> Bool) -> [a] -> [[a]]
breaks p as = case break p as of
  (xs,_:ys) -> xs : breaks p ys
  (xs,[]) -> [xs]

