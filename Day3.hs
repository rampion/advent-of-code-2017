module Day3 where
import Data.List (scanl')
import Data.Map
import Data.Maybe (catMaybes)
{-
--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite
two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location
marked 1 and then counting up while spiraling outward. For example, the first
few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data
must be carried back to square 1 (the location of the only access port for this
memory system) by programs that can only move up, down, left, or right. They
always take the shortest path: the Manhattan Distance between the location of
the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in
your puzzle input all the way to the access port?
-}

-- |
-- >>> part1 1
-- 0
-- >>> part1 12
-- 3
-- >>> part1 23
-- 2
-- >>> part1 1024
-- 31
part1 :: Integer -> Integer
part1 n = x + y where
  r = floor . sqrt $ fromIntegral n
  r2 = r * r
  -- r^2 <= n < (r+1)^2 = r^2 + r + (r + 1)
  x = abs $ (r - if odd r then 1 else 2) `div` 2 + if n == r2 then 0 else (1 + min 0 (r2 + r + 1 - n))
  y = abs $ (r - if odd r then 1 else 0) `div` 2 + if n == r2 then 0 else (r2 + 1 - min (r2 + r + 1) n)

{-
--- Part Two ---

As a stress test on the system, the programs here clear the grid and then store
the value 1 in square 1. Then, in the same allocation order as shown above,
they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

    Square 1 starts with the value 1.
    Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
    Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
    Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
    Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

Once a square is written, its value does not change. Therefore, the first few
squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
-}
{-
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26 
362  747  806  880  931 957
-}

xs :: [Integer]
xs = scanl' (+) 0 $ [0..] >>= \n -> concat
  [ replicate (2*n+1) 1
  , replicate (2*n+1) 0
  , replicate (2*n+2) (-1)
  , replicate (2*n+2) 0
  ]

ys :: [Integer]
ys = scanl' (+) 0 $ [0..] >>= \n -> concat
  [ replicate (2*n+1) 0
  , replicate (2*n+1) 1
  , replicate (2*n+2) 0
  , replicate (2*n+2) (-1)
  ]

ps :: [(Integer,Integer)]
ps = zip xs ys

vs :: [Integer]
vs = 1 : zipWith lookAround (tail ps) (tail ms)

ms :: [Map (Integer,Integer) Integer]
ms = scanl' (\m (p,v) -> insert p v m) empty $ zip ps vs

lookAround :: (Integer,Integer) -> Map (Integer,Integer) Integer -> Integer
lookAround (x,y) m = sum $ catMaybes [ Data.Map.lookup (x',y') m | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1] ]
