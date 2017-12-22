{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Test.QuickCheck

main :: IO ()
main = do
  Right ds <- parseFromFile (direction `sepBy` char ',') "Day11.input"
  print $ part1 ds

data Direction = N | NE | SE | S | SW | NW
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Arbitrary Direction where
  shrink = shrinkNothing
  arbitrary = arbitraryBoundedEnum
instance CoArbitrary Direction

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

data Coord = Coord { v :: Int, d :: Int }
  deriving (Eq, Show, Generic)
instance CoArbitrary Coord

instance Arbitrary Coord where
  arbitrary = Coord <$> arbitrary <*> arbitrary
  shrink (Coord v d) = 
    [ Coord v' d | v' <- [1 - abs v .. abs v - 1] ] ++
    [ Coord v d' | d' <- [1 - abs d .. abs d - 1] ]

instance Monoid Coord where
  mempty = Coord 0 0
  Coord v d `mappend` Coord v' d' = Coord (v + v') (d + d')

toCoord :: [Direction] -> Coord
toCoord = foldMap coord

coord :: Direction -> Coord
coord d = case d of
  N   -> Coord ( 1) ( 0)
  NE  -> Coord ( 0) (-1)
  SE  -> Coord (-1) (-1)
  S   -> Coord (-1) ( 0)
  SW  -> Coord ( 0) ( 1)
  NW  -> Coord ( 1) ( 1)

-- |
-- prop> toCoord (fromCoord c) == c
-- prop> length (fromCoord (toCoord ds)) <= length ds
fromCoord :: Coord -> [Direction]
fromCoord = concatMap (uncurry $ flip replicate) . directions

directions :: Coord -> [(Direction, Int)]
directions (Coord v d) = [(N, n), (NE, ne), (SE, se), (S, s), (SW, sw), (NW, nw)]
  where (n',s') = biAbs v
        (sw',ne') = biAbs d
        (ne, se, s) = minDiff ne' s'
        (sw, nw, n) = minDiff sw' n'

-- |
-- prop> biAbs x == (max 0 x, max 0 (negate x))
biAbs :: Int -> (Int, Int)
biAbs x | x >= 0    = (x, 0)
        | otherwise = (0, negate x)

-- |
-- prop> minDiff x y == (x - min x y, min x y, y - min x y)
minDiff :: Int -> Int -> (Int, Int, Int)
minDiff x y = case compare x y of
  LT -> (0, x, y - x)
  EQ -> (0, x, 0)
  GT -> (x - y, y, 0)

-- |
-- prop> distance c == length (fromCoord c)
distance :: Coord -> Int
distance = sum . map snd . directions

origin :: Coord
origin = Coord 0 0

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
part1 = distance . toCoord

part2 :: [Direction] -> Int
part2 = maximum . map distance . scanl (\c d -> c `mappend` coord d) origin

-- |
-- >>> simplify [NE,NE,NE]
-- [NE,NE,NE]
-- >>> simplify [NE,NE,SW,SW]
-- []
-- >>> simplify [NE,NE,S,S]
-- [SE,SE]
-- >>> simplify [SE,SW,SE,SW,SW]
-- [S,S,SW]
--
-- prop> (simplify . simplify) ds == simplify ds
simplify :: [Direction] -> [Direction]
simplify = fromCoord . toCoord
