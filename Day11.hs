{-# LANGUAGE LambdaCase #-}
module Main where
{-
--- Day 11: Hex Ed ---
Crossing the bridge, you've barely reached the other side of the stream when a
program comes up to you, clearly in distress. "It's my child process," she says,
"he's gotten lost in an infinite grid!"
Fortunately for her, you have plenty of experience with infinite grids.
Unfortunately for you, it's a hex grid.
The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be
found to the north, northeast, southeast, south, southwest, and northwest:
  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \
You have the path the child process took. Starting where he started, you need to
determine the fewest number of steps required to reach him. (A "step" means to
move from the hex you are in to any adjacent hex.)
For example:
    ne,ne,ne is 3 steps away.
    ne,ne,sw,sw is 0 steps away (back where you started).
    ne,ne,s,s is 2 steps away (se,se).
    se,sw,se,sw,sw is 3 steps away (s,s,sw).
-}
import Text.Parsec
import Text.Parsec.String (parseFromFile)

import qualified Data.Map as M
import qualified Data.List as L

main :: IO ()
main = parseFromFile (direction `sepBy` char ',') "Day11.input" >>= \case
  Left e -> fail (show e)
  Right ds -> print (part2 ds)

data Direction = N | NE | SE | S | SW | NW
  deriving (Eq, Ord, Show)

direction :: Parsec String () Direction
direction = 
  char 'n' *> 
    (   NE <$ char 'e' 
    <|> NW <$ char 'w'
    <|> pure N
    )
  <|>
  char 's' *>
    (   SE <$ char 'e'
    <|> SW <$ char 'w'
    <|> pure S
    )

-- |
-- >>> part1 [NE,NE,NE]
-- 3
-- >>> part1 [NE,NE,SW,SW]
-- 0
-- >>> part1 [NE,NE,S,S]
-- 2
-- >>> part1 [SE,SW,SE,SW,SW]
-- 3
part1 :: [Direction] -> Int
part1 = length . simplify

part2 :: [Direction] -> Int
part2 = maximum . map part1 . L.inits

-- |
-- >>> simplify [NE,NE,NE]
-- [NE,NE,NE]
-- >>> simplify [NE,NE,SW,SW]
-- []
-- >>> simplify [NE,NE,S,S]
-- [SE,SE]
-- >>> simplify [SE,SW,SE,SW,SW]
-- [S,S,SW]
simplify :: [Direction] -> [Direction]
simplify = expand . zoom . build

build :: [Direction] -> M.Map Direction Int
build = L.foldl' (\m d -> M.insertWith (+) d 1 m) blank

blank :: M.Map Direction Int
blank = M.fromList [ (N, 0), (NE, 0), (SE, 0), (S, 0), (SW, 0), (NW, 0) ]

jump :: M.Map Direction Int -> M.Map Direction Int
jump = step N NE SE . step NE SE S . step SE S SW
     . step S SW NW . step SW NW N . step NW N NE
  where
    step l c r m = M.insert l (m M.! l - delta)
                 . M.insert c (m M.! c + delta)
                 . M.insert r (m M.! r - delta)
                 $ m where
      delta = min (m M.! l) (m M.! r)


zoom :: M.Map Direction Int -> M.Map Direction Int
zoom m | m == m' = m
       | otherwise = zoom m'
  where m' = jump m

expand :: M.Map Direction Int -> [Direction]
expand = \m -> get N S m ++ get NE SW m ++ get NW SE m where
  get d d' m = case compare n 0 of
      GT -> replicate n d
      LT -> replicate (negate n) d'
      EQ -> []
    where n = m M.! d - m M.! d'
