{-
--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be
dancing.

There are sixteen programs in total, named a through p. They start by standing
in a line: a stands in position 0, b stands in position 1, and so on until p,
which stands in position 15.

The programs' dance consists of a sequence of dance moves:

    - Spin, written sX, makes X programs move from the end to the front, but
      maintain their order otherwise. (For example, s3 on abcde produces cdeab).
    - Exchange, written xA/B, makes the programs at positions A and B swap
      places.
    - Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do
the following dance:

    s1, a spin of size 1: eabcd.
    x3/4, swapping the last two programs: eabdc.
    pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle
input). In what order are the programs standing after their dance?
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad (guard)
import Text.Parsec
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  Right dance <- parseFromFile danceP "Day16.input"
  print $ initial 16
  print $ runDance dance (initial 16)
  let period = getPeriod dance 16
  putStrLn $ "period = " ++ show period
  let numDances = 1000000000 `rem` period
  putStrLn $ "numDances = " ++ show numDances
  let dance' = concat $ replicate numDances dance
  print $ runDance dance' (initial 16)


type Dance = [Move]

data Move
  = Spin Int
  | Exchange Position Position
  | Partner Program Program
  deriving (Show, Eq)

newtype Position = Position { fromPosition :: Int }
  deriving (Show, Eq)
newtype Program = Program { fromProgram :: Int }
  deriving (Show, Eq)

-- X
-- >>> parse danceP "test input" "s2,x3/4,pe/b"
-- Right [Spin 1, Exchange (Position 3) (Position 4), Partner (Program 4) (Program 1)]
danceP :: Parsec String () Dance
danceP = moveP `sepBy` char ',' where
  moveP, spinP, exchangeP, partnerP :: Parsec String () Move
  moveP = spinP <|> exchangeP <|> partnerP
  spinP = Spin . read <$> (char 's' *> many1 digit)
  exchangeP = Exchange <$> (char 'x' *> positionP) <*> (char '/' *> positionP)
  partnerP = Partner <$> (char 'p' *> programP) <*> (char '/' *> programP)

  positionP :: Parsec String () Position
  positionP = Position . read <$> many1 digit

  programP :: Parsec String () Program
  programP = Program . (subtract $ fromEnum 'a') . fromEnum <$> oneOf ['a'..'z']

data Promenade = Promenade 
  { programs :: !(V.Vector Program)
  , positions :: !(V.Vector Position)
  }
  deriving (Eq)

-- |
-- >>> initial 5
-- abcde
-- >>> initial 16
-- abcdefghijklmnop
initial :: Int -> Promenade
initial i = Promenade
  { programs = Program <$> V.enumFromTo 0 (i - 1)
  , positions = Position <$> V.enumFromTo 0 (i - 1)
  }

instance Show Promenade where
  show (Promenade {..}) = [ toEnum $ x + fromEnum 'a' | Program x <- V.toList programs ]

-- |
-- >>> read "cdeab" :: Promenade
-- cdeab
instance Read Promenade where
  readsPrec _ cs = do
    (xs@(_:_), cs) <- lex cs
    guard . all id . zipWith (==) ['a'..] $ L.sort xs
    let programs = V.fromList [ Program $ fromEnum x - fromEnum 'a' | x <- xs ]
    let positions = V.create $ do
          v <- M.new (length xs)
          V.imapM_ (\i (Program x) -> M.write v x (Position i)) programs
          return v
    return (Promenade programs positions, cs)

-- |
-- >>> runMove (Spin 1) (initial 5)
-- eabcd
-- >>> runMove (Exchange (Position 3) (Position 4)) it
-- eabdc
-- >>> runMove (Partner (Program 4) (Program 1)) it
-- baedc
runMove :: Move -> Promenade -> Promenade
runMove (Spin n) (Promenade {..}) = Promenade
  { programs = yv V.++ xv
  , positions = fmap (\(Position i) -> Position $ if i >= m - n then i - m + n else i + n) positions
  } where (xv,yv) = V.splitAt (m - n) programs
          m = V.length positions
runMove (Exchange i j) (Promenade {..}) = Promenade
  { programs = programs V.// [(fromPosition i, y), (fromPosition j, x)]
  , positions = positions V.// [(fromProgram x, j), (fromProgram y, i)]
  } where x = programs V.! fromPosition i
          y = programs V.! fromPosition j
runMove (Partner x y) (Promenade {..}) = Promenade
  { programs = programs V.// [(fromPosition i, y), (fromPosition j, x)]
  , positions = positions V.// [(fromProgram x, j), (fromProgram y, i)]
  } where i = positions V.! fromProgram x
          j = positions V.! fromProgram y

-- |
-- >>> runDance [Spin 1, Exchange (Position 3) (Position 4), Partner (Program 4) (Program 1)] (initial 5)
-- baedc
-- >>> runDance [Spin 1, Exchange (Position 3) (Position 4), Partner (Program 4) (Program 1)] it
-- ceadb
runDance :: Dance -> Promenade -> Promenade
runDance = flip $ L.foldl' (flip runMove)

-- |
-- >>> getPeriod [] 5
-- 1
-- >>> getPeriod [Spin 1] 5
-- 5
-- >>> getPeriod [Exchange (Position 3) (Position 4)] 5
-- 2
-- >>> getPeriod [Partner (Program 4) (Program 1)] 5
-- 2
-- >>> getPeriod [Spin 1, Exchange (Position 3) (Position 4), Partner (Program 4) (Program 1)] 5
-- 4
getPeriod :: Dance -> Int -> Int
getPeriod d n = succ . length . takeWhile (/= initial n) . tail $ iterate (runDance d) (initial n)
{-
Now that you're starting to get a feel for the dance moves, you turn your
attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance, the programs
perform it again and again: including the first dance, a total of one billion
(1000000000) times.

In the example above, their second dance would begin with the order baedc, and
use the same dance moves:

    s1, a spin of size 1: cbaed.
    x3/4, swapping the last two programs: cbade.
    pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?
-}

