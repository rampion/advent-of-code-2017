{-
--- Day 22: Sporifica Virus ---

Diagnostics indicate that the local grid computing cluster has been contaminated
with the Sporifica Virus. The grid computing cluster is a seemingly-infinite
two-dimensional grid of compute nodes. Each node is either clean or infected by
the virus.

To prevent overloading the nodes (which would render them useless to the virus)
or detection by system administrators, exactly one virus carrier moves through
the network, infecting or cleaning nodes as it moves. The virus carrier is
always located on a single node in the network (the current node) and keeps
track of the direction it is facing.

To avoid detection, the virus carrier works in bursts; in each burst, it wakes
up, does some work, and goes back to sleep. The following steps are all executed
in order one time each burst:

    If the current node is infected, it turns to its right. Otherwise, it turns
    to its left. (Turning is done in-place; the current node does not change.)

    If the current node is clean, it becomes infected. Otherwise, it becomes
    cleaned. (This is done after the node is considered for the purposes of
    changing direction.)

    The virus carrier moves forward one node in the direction it is facing.

Diagnostics have also provided a map of the node infection status (your puzzle
input). Clean nodes are shown as .; infected nodes are shown as #. This map only
shows the center of the grid; there are many more nodes beyond those shown, but
none of them are currently infected.

The virus carrier begins in the middle of the map facing up.

For example, suppose you are given a map like this:

..#
#..
...

Then, the middle of the infinite grid looks like this, with the virus carrier's
position marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on a clean node, so it turns left, infects the node, and
moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]# . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on an infected node, so it turns right, cleans the node,
and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . . # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Four times in a row, the virus carrier finds a clean, infects it, turns left,
and moves forward, ending in the same place and still facing up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . #[#]. # . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Now on the same node as before, it sees an infection, which causes it to turn
right, clean the node, and move forward:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . # .[.]# . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

After the above actions, a total of 7 bursts of activity had taken place. Of
them, 5 bursts of activity caused an infection.

After a total of 70, the grid looks like this, with the virus carrier facing up:

. . . . . # # . .
. . . . # . . # .
. . . # . . . . #
. . # . #[.]. . #
. . # . # . . # .
. . . . . # # . .
. . . . . . . . .
. . . . . . . . .

By this time, 41 bursts of activity caused an infection (though most of those
nodes have since been cleaned).

After a total of 10000 bursts of activity, 5587 bursts will have caused an
infection.

Given your actual map, after 10000 bursts of activity, how many bursts cause a
node to become infected? (Do not count nodes that begin infected.)
-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Applicative
import Control.Monad.State

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import qualified Data.List as L

main :: IO ()
main = do
  Right grid <- P.parseFromFile gridP "Day22.input"
  -- print $ part1 grid
  print $ part2 grid

data NodeState = Weakened | Infected | Flagged
  deriving Eq

example :: Grid
example = M.fromList [ ((1,1),Infected), ((-1,0),Infected) ]

-- |
-- >>> Right g = P.parse gridP "example input" $ unlines [ "..#", "#..", "..." ]
-- >>> example == g
-- True
gridP :: P.Parsec String () Grid
gridP = do
  let nodeP =   Nothing <$ P.char '.'
            <|> Just Weakened <$ P.char 'W'
            <|> Just Infected <$ P.char '#'
            <|> Just Flagged  <$ P.char 'F'
  rows <- P.many1 nodeP `P.endBy` P.newline
  let n = length rows
  guard $ odd n
  guard $ all (\r -> length r == n) rows
  let q = n `div` 2
  return . M.fromList $ do
    (y,r) <- zip [q,q-1..negate q] rows 
    (x,Just s) <- zip [negate q..q] r
    return ((x,y), s)

type Grid = M.Map (Int,Int) NodeState

-- |
-- >>> part1 example
-- 5587
part1 :: Grid -> Int
part1 = sum . evalState (replicateM 10000 $ fromEnum <$> slowInfection) . initial

-- Xslow
-- >>> part2 example
-- 2511944
part2 :: Grid -> Int
part2 = sum . evalState (replicateM 10000000 $ fromEnum <$> fastInfection) . initial

initial :: Grid -> Carrier
initial g = Carrier g 0 0 0 1

data Carrier = Carrier
  { grid :: !Grid
  , px   :: !Int
  , py   :: !Int
  , dx   :: !Int
  , dy   :: !Int
  }

-- |
-- >>> print (initial example)
--  . . #
--  #[.].
--  . . .
instance Show Carrier where
  show (Carrier {..}) = L.intercalate "\n" $ do
    let (xs, ys) = unzip $ M.keys grid
    let hi = maximum . fmap abs $ px : py : xs ++ ys
    let lo = negate hi
    y <- [hi,hi - 1..lo]
    return . foldr ($) [] $ do
      x <- [lo..hi]
      let sym = case M.lookup (x,y) grid of
            Nothing       -> '.'
            Just Weakened -> 'W'
            Just Infected -> '#'
            Just Flagged  -> 'F'
      return $ if px == x && py == y
        then \suf -> case suf of
              []        -> '[' : sym : ']' : []
              ~(' ':xt) -> '[' : sym : ']' : xt
        else (' ':) . (sym:)
              
-- |
-- >>> slowInfection `evalState` initial example
-- True
-- >>> slowInfection `execState` initial example
--  . . #
-- [#]# .
--  . . .
-- >>> slowInfection `execState` it
-- [.]. #
--  . # .
--  . . .
-- >>> replicateM 4 slowInfection `execState` it
--  . . . . .
--  #[#]. # .
--  # # # . .
--  . . . . .
--  . . . . .
-- >>> replicateM 70 slowInfection `execState` initial example
--  . . . . . # # . .
--  . . . . # . . # .
--  . . . # . . . . #
--  . . # . #[.]. . #
--  . . # . # . . # .
--  . . . . . # # . .
--  . . . . . . . . .
--  . . . . . . . . .
--  . . . . . . . . .
slowInfection :: State Carrier Bool
slowInfection = state $ \(Carrier {..}) -> 
  let p = (px,py)
      (toggle,dx',dy',retval)
        | p `M.member` grid = (M.delete, dy, negate dx, False)
        | otherwise         = (flip M.insert Infected, negate dy, dx, True)
  in  (retval, Carrier 
          { grid = toggle p grid
          , px = px + dx'
          , py = py + dy'
          , dx = dx'
          , dy = dy'
          }
      )


{-
--- Part Two ---

As you go to remove the virus from the infected nodes, it evolves to resist your
attempt.

Now, before it infects a clean node, it will weaken it to disable your defenses.
If it encounters an infected node, it will instead flag the node to be cleaned
in the future. So:

    Clean nodes become weakened.
    Weakened nodes become infected.
    Infected nodes become flagged.
    Flagged nodes become clean.

Every node is always in exactly one of the above states.

The virus carrier still functions in a similar way, but now uses the following
logic during its bursts of action:

    Decide which way to turn based on the current node:
        If it is clean, it turns left.

        If it is weakened, it does not turn, and will continue moving in the
        same direction.

        If it is infected, it turns right.

        If it is flagged, it reverses direction, and will go back the way it came.
    Modify the state of the current node, as described above.
    The virus carrier moves forward one node in the direction it is facing.

Start with the same map (still using . for clean and # for infected) and still
with the virus carrier starting in the middle and facing up.

Using the same initial state as the previous example, and drawing weakened as W
and flagged as F, the middle of the infinite grid looks like this, with the
virus carrier's position again marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

This is the same as before, since no initial nodes are weakened or flagged. The
virus carrier is on a clean node, so it still turns left, instead weakens the
node, and moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on an infected node, so it still turns right, instead flags
the node, and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . F W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

This process repeats three more times, ending on the previously-flagged node and
facing right:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. . W[F]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Finding a flagged node, it reverses direction and cleans the node:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. .[W]. W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The weakened node becomes infected, and it continues in the same direction:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
.[.]# . W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Of the first 100 bursts, 26 will result in infection. Unfortunately, another
feature of this evolved virus is speed; of the first 10000000 bursts, 2511944
will result in infection.

Given your actual map, after 10000000 bursts of activity, how many bursts cause
a node to become infected? (Do not count nodes that begin infected.)
-}
-- |
-- >>> fastInfection `execState` initial example
--  . . #
-- [#]W .
--  . . .
-- >>> fastInfection `execState` it
-- [.]. #
--  F W .
--  . . .
-- >>> replicateM 3 fastInfection `execState` it
--  . . . . .
--  W W . # .
--  W[F]W . .
--  . . . . .
--  . . . . .
-- >>> fastInfection `execState` it
--  . . . . .
--  W W . # .
-- [W]. W . .
--  . . . . .
--  . . . . .
-- >>> fastInfection `execState` it
--  . . . . . . .
--  . . . . . . .
--  . W W . # . .
-- [.]# . W . . .
--  . . . . . . .
--  . . . . . . .
--  . . . . . . .
-- >>> sum $ replicateM 100 (fromEnum <$> fastInfection) `evalState` initial example
-- 26
fastInfection :: State Carrier Bool
fastInfection = state $ \(Carrier {..}) -> 
  let p = (px,py)
      (toggle,dx',dy',retval) = case M.lookup p grid of
        Nothing       -> (flip M.insert Weakened, negate dy, dx, False)
        Just Weakened -> (flip M.insert Infected, dx, dy, True)
        Just Infected -> (flip M.insert Flagged, dy, negate dx, False)
        Just Flagged  -> (M.delete, negate dx, negate dy, False)
  in  (retval, Carrier 
          { grid = toggle p grid
          , px = px + dx'
          , py = py + dy'
          , dx = dx'
          , dy = dy'
          }
      )
