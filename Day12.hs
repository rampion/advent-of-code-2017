{-# LANGUAGE LambdaCase #-}
module Main where
{-
--- Day 12: Digital Plumber ---

Walking along the memory banks of the stream, you find a small village that is
experiencing a little confusion: some programs can't communicate with each
other.

Programs in this village communicate using a fixed system of pipes. Messages
are passed between programs using these pipes, but most programs aren't
connected to each other directly. Instead, programs pass messages between each
other until the message reaches the intended recipient.

For some reason, though, some of these messages aren't ever reaching their
intended recipient, and the programs suspect that some pipes are missing. They
would like you to investigate.

You walk through the village and record the ID of each program and the IDs with
which it can communicate directly (your puzzle input). Each program has one or
more programs with which it can communicate, and these pipes are bidirectional;
if 8 says it can communicate with 11, then 11 will say it can communicate with
8.

You need to figure out how many programs are in the group that contains program
ID 0.

For example, suppose you go door-to-door like a travelling salesman and record the following list:

0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5

In this example, the following programs are in the group that contains program ID 0:

    Program 0 by definition.
    Program 2, directly connected to program 0.
    Program 3 via program 2.
    Program 4 via program 2.
    Program 5 via programs 6, then 4, then 2.
    Program 6 via programs 4, then 2.

Therefore, a total of 6 programs are in this group; all but program 1, which
has a pipe that connects it to itself.

How many programs are in the group that contains program ID 0?
-}
import Control.Monad.State
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Set as S
import qualified Data.IntMap as M

main :: IO ()
main = parseFromFile graph "Day12.input" >>= \case
  Left e -> fail (show e)
  Right g -> print . length $ components g

type Graph = M.IntMap [Int]

graph :: Parsec String () Graph
graph = M.fromList <$> (adjacency `endBy` char '\n') where
  adjacency = (,) <$> integer <* string " <-> " <*> (integer `sepBy` string ", ")
  integer = read <$> many1 digit

-- |
-- >>> g = M.fromList [(0,[2]),(1,[1]),(2,[0,3,4]),(3,[2,4]),(4,[2,3,6]),(5,[6]),(6,[4,5])]
-- >>> bfs 0 g
-- [0,2,3,4,6,5]
bfs :: Int -> Graph -> [Int]
bfs = \seed g -> let queue = seed : search g (S.singleton seed) 1 queue in queue where
  search _ _ 0 ~[] = []
  search g seen n (x:xs) = 
    let (unseen, seen') = filterM (\y -> gets (not . S.member y) <* modify (S.insert y)) (g M.! x) `runState` seen
    in unseen ++ search g seen' (n - 1 + length unseen) xs


{-
--- Part Two ---

There are more programs than just the ones in the group containing program ID
0. The rest of them have no way of reaching that group, and still might have no
way of reaching each other.

A group is a collection of programs that can all communicate via pipes either
directly or indirectly. The programs you identified just a moment ago are all
part of the same group. Now, they would like you to determine the total number
of groups.

In the example above, there were 2 groups: one consisting of programs
0,2,3,4,5,6, and the other consisting solely of program 1.

How many groups are there in total?
-}

components :: Graph -> [Int]
components = \g -> search g (M.keys g) S.empty where
  search g [] seen = []
  search g (x : xs) seen
    | x `S.member` seen = search g xs seen
    | otherwise         = x : search g xs (foldr S.insert seen $ bfs x g)
