{-
--- Day 19: A Series of Tubes ---

Somehow, a network packet got lost and ended up here. It's trying to follow a
routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -,
and +) show the path it needs to take, starting by going down onto the only
line connected to the top of the diagram. It needs to follow this path until it
reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to
continue going the same direction, and only turn left or right when there's no
other option. In addition, someone has left letters on the line; these also
don't change its direction, but it can use them to keep track of where it's
been. For example:

     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 

Given this diagram, the packet needs to take the following path:

- Starting at the only line touching the top of the diagram, it must go down,
  pass through A, and continue onward to the first +.
- Travel right, up, and right, passing through B in the process.
- Continue down (collecting C), right, and up (collecting D).
- Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What
letters will it see (in the order it would see them) if it follows the path?
(The routing diagram is very wide; make sure you view it without line
wrapping.)
-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Monoid (Sum(..))
import Control.Arrow (first)

type Grid = V.Vector (V.Vector Char)

exampleGrid :: Grid
exampleGrid = grid $ unlines
  [ "     |          "
  , "     |  +--+    "
  , "     A  |  C    "
  , " F---|----E|--+ "
  , "     |  |  |  D "
  , "     +B-+  +--+ "
  , "                "
  ]

grid :: String -> Grid
grid = V.fromList . map V.fromList . lines

-- |
-- >>> startPosition exampleGrid
-- 5
startPosition :: Grid -> Int
startPosition = fromJust . V.findIndex (=='|') . (V.! 0)

-- |
-- >>> followPath exampleGrid
-- (38,"ABCDEF")
followPath :: Grid -> (Int, String)
followPath g = first getSum $ go 1 (startPosition g) 1 0 1 where
  go !n x y dx dy = case (g V.! y) V.! x of
    '|' -> go (n + 1) (x + dx) (y + dy) dx dy
    '-' -> go (n + 1) (x + dx) (y + dy) dx dy
    ' ' -> (n,[])
    '+' -> (++) 
           <$> go 1 (x + dy) (y + dx) dy dx
           <*> go n (x - dy) (y - dx) (negate dy) (negate dx)
    cha -> (cha:) <$> go (n+1) (x + dx) (y + dy) dx dy

main :: IO ()
main = do
  print . followPath . grid =<< readFile "Day19.input"

{-
--- Part Two ---

The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 

...the packet would go:

    6 steps down (including the first line at the top of the diagram).
    3 steps right.
    4 steps up.
    3 steps right.
    4 steps down.
    3 steps right.
    2 steps up.
    13 steps left (including the F it stops on).

This would result in a total of 38 steps.

How many steps does the packet need to go?
 -}
