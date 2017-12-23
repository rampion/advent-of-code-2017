{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)

{-
--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent assistance
with jump instructions, it would like you to compute the result of a series of
unusual register instructions.

Each instruction consists of several parts: the register to modify, whether to
increase or decrease that register's value, the amount by which to increase or
decrease it, and a condition. If the condition fails, skip the instruction
without modifying the register. The registers all start at 0. The instructions
look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10

These instructions would be processed as follows:

    Because a starts at 0, it is not greater than 1, and so b is not modified.
    a is increased by 1 (to 1) because b is less than 5 (it is 0).
    c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
    c is increased by -20 (to -10) because c is equal to 10.

After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the registers
are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions in
your puzzle input?
-}

import Text.Parsec

data Instruction = Instruction
  { target  :: Register
  , update   :: Int -> Int
  , examine   :: Register
  , predicate :: Int -> Bool
  }

newtype Register = Register { name :: String }
  deriving (Ord, Eq)

register :: Parsec String () Register
register = Register <$> many1 letter

instruction :: Parsec String () Instruction 
instruction = Instruction 
    <$> register 
    <* char ' ' 
    <*> (increase <|> decrease)
    <* string " if "
    <*> register 
    <* char ' '
    <*> (try ge <|> gt <|> try le <|> lt <|> ne <|> eq)
  where
    number = read <$> ((++) <$> option "" (string "-") <*> many1 digit)
    increase = (+) <$> (string "inc " *> number)
    decrease = subtract <$> (string "dec " *> number)
    ge = flip (>=) <$> (string ">= " *> number)
    gt = flip (>) <$> (string "> " *> number)
    le = flip (<=) <$> (string "<= " *> number)
    lt = flip (<) <$> (string "< " *> number)
    ne = flip (/=) <$> (string "!= " *> number)
    eq = flip (==) <$> (string "== " *> number)

type Registers = M.Map Register Int

execute :: Instruction -> Registers -> Registers
execute (Instruction {..}) m | predicate (fromMaybe 0 $ M.lookup examine m) = M.insertWith (const update) target (update 0) m
                             | otherwise = m
  
part1 :: [Instruction] -> Int
part1 = maximum . M.elems . L.foldl' (flip execute) M.empty

{-
--- Part Two ---

To be safe, the CPU also needs to know the highest value held in any register
during this process so that it can decide how much memory to allocate to these
operations. For example, in the above instructions, the highest value ever held
was 10 (in register c after the third instruction was evaluated).
-}

part2 :: [Instruction] -> Int
part2 = maximum . fmap (maximum . M.elems) . tail . L.scanl' (flip execute) M.empty

main :: IO ()
main = parse (instruction `endBy` newline) "Day8.input" <$> readFile "Day8.input" >>= \case
  Left e -> fail (show e)
  Right instructions -> print (part2 instructions)
