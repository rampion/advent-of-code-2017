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
import Data.Bits
import Control.Monad
import qualified Data.List as L
import Text.Parsec

main :: IO ()
main = return ()

type Word16 = ((Word4,Word4),(Word4,Word4))
type Rule4 = (Word4, Word9)
type Rule9 = (Word9, Word16)

rules :: Parsec String () ([Rule4],[Rule9])
rules = (,) <$> (rule4 `endBy` newline) <*> (rule9 `endBy` newline)

rule4 :: Parsec String () Rule4
rule4 = try $ (,) <$> word4 <*> (string " => " *> word9)

rule9 :: Parsec String () Rule9
rule9 = try $ (,) <$> word9 <*> (string " => " *> word16)

word4 :: Parsec String () Word4
word4 = Word4 $ pattern 2

word9 :: Parsec String () Word9
word9 = Word9 $ pattern 3

word16 :: Parsec String () Word16
word16 = undefined

-- x
-- prop> permute [0..7] [0,1,2,3,4,5,6,7] a == (a :: W.Word8)
-- prop> permute [0..7] [1,2,3,4,5,6,7,0] a == (a :: W.Word8) `rotateL` 1
-- prop> permute [0..7] [7,0,1,2,3,4,5,6] a == (a :: W.Word8) `rotateR` 1
permute :: (Num a, Bits a) => [Int] -> [Int] -> a -> a
permute is js = L.foldl' (.|.) 0 . zipWithM mv is js
  where mv i j a = (a .&. bit i) `shift` (j - i)

-- 8 7 6
-- 5 4 3
-- 2 1 0
newtype Word9 = Word9 Int
  deriving Show
getWord9 :: Word9 -> Int
getWord9 (Word9 x) = x

-- |
-- >>> (getWord9 . clockwise9 . Word9) 0
-- 0
-- >>> (getWord9 . clockwise9 . Word9) 1
-- 4
-- >>> (getWord9 . clockwise9 . Word9) 4
-- 256
-- >>> (getWord9 . clockwise9 . Word9) 256
-- 64
-- >>> (getWord9 . clockwise9 . Word9) 511
-- 511
--
clockwise9, widdershins9, vertical9, horizontal9 :: Word9 -> Word9
--                          --  [0,1,2,3,4,5,6,7,8]
clockwise9    = Word9 . permute [0..8] [2,5,8,1,4,7,0,3,6] . getWord9
widdershins9  = Word9 . permute [0..8] [6,3,0,7,4,1,8,5,2] . getWord9
vertical9     = Word9 . permute [0..8] [6,7,8,3,4,5,0,1,2] . getWord9
horizontal9   = Word9 . permute [0..8] [2,1,0,5,4,3,8,7,6] . getWord9

-- 3 2
-- 1 0
newtype Word4 = Word4 Int
  deriving Show
getWord4 :: Word4 -> Int
getWord4 (Word4 x) = x

-- >>> (getWord4 . clockwise4 . Word4) 0
-- 0
-- >>> (getWord4 . clockwise4 . Word4) 1
-- 2
-- >>> (getWord4 . clockwise4 . Word4) 2
-- 8
-- >>> (getWord4 . clockwise4 . Word4) 4
-- 1
-- >>> (getWord4 . clockwise4 . Word4) 8
-- 4
-- >>> (getWord4 . clockwise4 . Word4) 15
-- 15
clockwise4, widdershins4, vertical4, horizontal4 :: Word4 -> Word4
--                          --  [0,1,2,3]
clockwise4    = Word4 . permute [0..3] [1,3,0,2] . getWord4
widdershins4  = Word4 . permute [0..3] [2,0,3,1] . getWord4
vertical4     = Word4 . permute [0..3] [1,0,3,2] . getWord4
horizontal4   = Word4 . permute [0..3] [2,3,0,1] . getWord4

-- |
-- >>> fourToNine ((Word4 0,Word4 0,Word4 0),(Word4 0,Word4 0,Word4 0),(Word4 0,Word4 0,Word4 15))
-- ((Word9 0,Word9 0),(Word9 0,Word9 27))
-- >>> fourToNine ((Word4 15,Word4 0,Word4 0),(Word4 0,Word4 0,Word4 0),(Word4 0,Word4 0,Word4 0))
-- ((Word9 432,Word9 0),(Word9 0,Word9 0))
fourToNine :: ((Word4,Word4,Word4)
              ,(Word4,Word4,Word4)
              ,(Word4,Word4,Word4)) ->  ((Word9,Word9)
                                        ,(Word9,Word9))
fourToNine ((Word4 a,Word4 b,Word4 c)
           ,(Word4 d,Word4 e,Word4 f)
           ,(Word4 g,Word4 h,Word4 i)) = 
  -- 3a 2a 3b  2b 3c 2c      8w 7w 6w  8x 7x 6x
  -- 1a 0a 1b  0b 1c 0c      5w 4w 3w  5x 4x 3x
  -- 3d 2d 3e  2e 3f 2f      2w 1w 0w  2x 1x 0x
  --                     =>
  -- 1d 0d 1e  0e 1f 0f      8y 7y 6y  8z 7z 6z
  -- 3g 2g 3h  2h 3i 2i      5y 4y 3y  5z 4z 3z
  -- 1g 0g 1h  0h 1i 0i      2y 1y 0y  2z 1z 0z
  ((Word9 $  permute [0..4] [4,5,7,8] a  .|. permute [1,3] [3,6] b 
         .|. permute [2,3] [1,2] d       .|. permute [3] [0] e
   ,Word9 $  permute [0,2] [8,5] b       .|. permute [0..4] [3,4,6,7] c 
         .|. permute [2] [2] e           .|. permute [2,3] [0,1] f
   )
  ,(Word9 $  permute [0,1] [7,8] d       .|. permute [1] [6] e
         .|. permute [0..4] [1,2,4,5] g  .|. permute [1,3] [0,3] h
   ,Word9 $  permute [0] [8] e           .|. permute [0,1] [6,7] f
         .|. permute [0,2] [2,5] h       .|. permute [0..4] [0,1,3,4] i
   )
  )

nineToFour :: ((Word9,Word9)
              ,(Word9,Word9)) -> ((Word4,Word4,Word4)
                                 ,(Word4,Word4,Word4)
                                 ,(Word4,Word4,Word4))
nineToFour ((Word9 w,Word9 x)
           ,(Word9 y,Word9 z)) =
  -- 8w 7w 6w  8x 7x 6x      3a 2a 3b  2b 3c 2c
  -- 5w 4w 3w  5x 4x 3x      1a 0a 1b  0b 1c 0c
  -- 2w 1w 0w  2x 1x 0x      3d 2d 3e  2e 3f 2f
  --                     =>                    
  -- 8y 7y 6y  8z 7z 6z      1d 0d 1e  0e 1f 0f
  -- 5y 4y 3y  5z 4z 3z      3g 2g 3h  2h 3i 2i
  -- 2y 1y 0y  2z 1z 0z      1g 0g 1h  0h 1i 0i
  ((Word4 $ permute [4,5,7,8] [0..4] w
   ,Word4 $ permute [3,6] [1,3] w .|. permute [8,5] [0,2] x
   ,Word4 $ permute [3,4,6,7] [0..4] x
   )
  ,(Word4 $ permute [1,2] [2,3] w .|. permute [7,8] [0,1] y
   ,Word4 $ permute [0] [3] w .|. permute [2] [2] x .|. permute [6] [1] y .|. permute [8] [0] z
   ,Word4 $ permute [0,1] [2,3] x .|. permute [6,7] [0,1] z
   )
  ,(Word4 $ permute [1,2,4,5] [0..4] y
   ,Word4 $ permute [0,3] [1,3] y .|. permute [2,5] [0,2] z
   ,Word4 $ permute [0,1,3,4] [0..4] z
   )
  )

part1 :: (Word4 -> Word9) -> (Word9 -> Word16) -> Int
part1 f g
  = sum . map (sum . map (popCount . getWord9))
  . map (map f)                 -- 18x18
  . quadMap g                   -- 12x12
  . map (map f) . ninesToFours  -- 9x9
  . map (map f)                 -- 6x6
  . quadMap g                   -- 4x4
  $ [[initial]]                 -- 3x3
  where initial = Word9 143

foursToNines :: [[Word4]] -> [[Word9]]
foursToNines = quadMap fourToNine . trip (zipWith3 (,,)) . map (trip (,,))

ninesToFours :: [[Word9]] -> [[Word4]]
ninesToFours = nonoMap nineToFour . pair zip . map (pair (,))

nonoMap :: (a -> ((b,b,b),(b,b,b),(b,b,b))) -> [[a]] -> [[b]]
nonoMap f = map untrip . untrip . map (unzip3 . map f)
-- |
-- >>> quadMap (\x -> (('a':x,'b':x),('c':x,'d':x))) [["0","1"],["2","3"]]
-- [["a0","b0","a1","b1"],["c0","d0","c1","d1"],["a2","b2","a3","b3"],["c2","d2","c3","d3"]]
quadMap :: (a -> ((b,b),(b,b))) -> [[a]] -> [[b]]
quadMap f = map unpair . unpair . map (unzip . map f)

splits :: Int -> [a] -> [[a]]
splits n = L.unfoldr $ \as -> case splitAt n as of
  ([], _) -> Nothing
  p -> Just p

pair :: (a -> a -> b) -> [a] -> [b]
pair f = map (\ ~[a,b] -> f a b) . splits 2

trip :: (a -> a -> a -> b) -> [a] -> [b]
trip f = map (\ ~[a,b,c] -> f a b c) . splits 3

unpair :: [(a,a)] -> [a]
unpair = foldr (\(a,b) -> (a:) . (b:)) []

untrip :: [(a,a,a)] -> [a]
untrip = foldr (\(a,b,c) -> (a:) . (b:) . (c:)) []
