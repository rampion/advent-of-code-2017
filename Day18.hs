{-
--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply
"Duet". Rather than bother the sound card with it, you decide to run the code
yourself. Unfortunately, you don't see any documentation, so you're left to
figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are
each named with a single letter and that can each hold a single integer. You
suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what
they do. Here's what you determine:

- snd X plays a sound with a frequency equal to the value of X.
- set X Y sets register X to the value of Y.
- add X Y increases register X by the value of Y.
- mul X Y sets register X to the result of multiplying the value contained in
  register X by the value of Y.
- mod X Y sets register X to the remainder of dividing the value contained in
  register X by the value of Y (that is, it sets X to the result of X modulo Y).
- rcv X recovers the frequency of the last sound played, but only when the value
  of X is not zero. (If it is zero, the command does nothing.)
- jgz X Y jumps with an offset of the value of Y, but only if the value of X is
  greater than zero. (An offset of 2 skips the next instruction, an offset of -1
  jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a
number. The value of a register is the integer it contains; the value of a
number is that number.

After each jump instruction, the program continues with the instruction to which
the jump jumped. After any other instruction, the program continues with the
next instruction. Continuing (or jumping) off either end of the program
terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2

- The first four instructions set a to 1, add 2 to it, square it, and then set
  it to itself modulo 5, resulting in a value of 4.
- Then, a sound with frequency 4 (the value of a) is played.
- After that, a is set to 0, causing the subsequent rcv and jgz instructions to
  both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
- Finally, a is set to 1, causing the next jgz instruction to activate, jumping
  back two instructions to another jump, which jumps again to the rcv, which
  ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound
played is 4.

What is the value of the recovered frequency (the value of the most recently
played sound) the first time a rcv instruction is executed with a non-zero
value?
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, catMaybes)
-- import Debug.Trace (traceShow)

main :: IO ()
main = parseFromFile program "Day18.input" >>= \case
  Left err -> fail $ show err
  Right prog -> do
    print $ part1 prog
    print $ part2 prog

program :: Parsec String () Program
program = V.fromList <$> instruction `endBy` newline

-- |
-- >>> parse instruction "example input" "set a 1"
-- Right (SET 'a' (Right 1))
-- >>> parse instruction "example input" "add a 2"
-- Right (ADD 'a' (Right 2))
-- >>> parse instruction "example input" "mul a a"
-- Right (MUL 'a' (Left 'a'))
-- >>> parse instruction "example input" "mod a 5"
-- Right (MOD 'a' (Right 5))
-- >>> parse instruction "example input" "snd a"
-- Right (SND (Left 'a'))
-- >>> parse instruction "example input" "set a 0"
-- Right (SET 'a' (Right 0))
-- >>> parse instruction "example input" "rcv a"
-- Right (RCV 'a')
-- >>> parse instruction "example input" "jgz a -1"
-- Right (JGZ (Left 'a') (Right (-1)))
-- >>> parse instruction "example input" "set a 1"
-- Right (SET 'a' (Right 1))
-- >>> parse instruction "example input" "jgz a -2"
-- Right (JGZ (Left 'a') (Right (-2)))
instruction :: Parsec String () Instruction
instruction = choice
  [ try $ SND <$> (string "snd " *> operand)
  ,       SET <$> (string "set " *> register) <*> (char ' ' *> operand)
  ,       ADD <$> (string "add " *> register) <*> (char ' ' *> operand)
  , try $ MUL <$> (string "mul " *> register) <*> (char ' ' *> operand)
  ,       MOD <$> (string "mod " *> register) <*> (char ' ' *> operand)
  ,       RCV <$> (string "rcv " *> register)
  ,       JGZ <$> (string "jgz " *> operand) <*> (char ' ' *> operand)
  ]

register :: Parsec String () Register
register = oneOf ['a'..'z']

operand :: Parsec String () Operand
operand = (Left <$> register) <|> (Right <$> integer)

integer :: Parsec String () Int
integer = (option id $ char '-' *> pure negate) <*> (read <$> many1 digit)


data Instruction
  = SND Operand
  | SET Register Operand
  | ADD Register Operand
  | MUL Register Operand
  | MOD Register Operand
  | RCV Register
  | JGZ Operand Operand
  deriving (Show, Eq)

type Program = V.Vector (Instruction)
type Register = Char
type Value = Int
type Operand = Either Register Value

example :: Program
example = V.fromList
  [ SET 'a' $ Right 1
  , ADD 'a' $ Right 2
  , MUL 'a' $ Left 'a'
  , MOD 'a' $ Right 5
  , SND $ Left 'a'
  , SET 'a' $ Right 0
  , RCV 'a'
  , JGZ (Left 'a') $ Right (-1)
  , SET 'a' $ Right 1
  , JGZ (Left 'a') $ Right (-2)
  ]

-- |
-- >>> part1 example
-- Just 4
part1 :: Program -> Maybe Int
part1 prog = run M.empty 0 Nothing where
  run m i prev
    | i < 0 || i >= V.length prog = Nothing
    | otherwise = -- traceShow (m,i,prog V.! i) $
        let val (Left j) = fromMaybe 0 $ M.lookup j m
            val (Right v) = v
        in case prog V.! i of
          SND x   -> run m (i + 1) (Just $ val x)
          SET x y -> run (M.insert x (val y) m) (i + 1) prev
          ADD x y -> run (M.insertWith (+) x (val y) m) (i + 1) prev
          MUL x y -> run (M.adjust (* val y) x m ) (i + 1) prev
          MOD x y -> run (M.adjust (`rem` val y) x m) (i + 1) prev
          RCV x | m M.! x == 0 -> run m (i + 1) prev
                | otherwise    -> prev
          JGZ x y | val x <= 0 -> run m (i + 1) prev
                  | otherwise  -> run m (i + val y) prev

{-
--- Part Two ---

As you congratulate yourself for a job well done, you notice that the
documentation has been on the back of the tablet this entire time. While you
actually got most of the instructions correct, there are a few key differences.
This assembly code isn't about sound at all - it's meant to be run twice at the
same time.

Each running copy of the program has its own set of registers and follows the
code independently - in fact, the programs don't even necessarily run at the
same speed. To coordinate, they use the send (snd) and receive (rcv)
instructions:

  - snd X sends the value of X to the other program. These values wait in a
    queue until that program is ready to receive them. Each program has its own
    message queue, so a program can never receive a message it sent.
  - rcv X receives the next value and stores it in register X. If no values are
    in the queue, the program waits for a value to be sent to it. Programs do
    not continue to the next instruction until they have received a value.
    Values are received in the order they are sent.

Each program also has its own program ID (one 0 and the other 1); the register p
should begin with this value.

For example:

snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d

Both programs begin by sending three values to the other. Program 0 sends 1, 2,
0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and
stores it in a, receives another value (both 2) and stores it in b, and then
each receives the program ID of the other program (program 0 receives 1; program
1 receives 0) and stores it in c. Each program now sees a different value in its
own copy of register c.

Finally, both programs try to rcv a fourth time, but no data is waiting for
either of them, and they reach a deadlock. When this happens, both programs
terminate.

It should be noted that it would be equally valid for the programs to run at
different speeds; for example, program 0 might have sent all three values and
then stopped at the first rcv before program 1 executed even its first
instruction.

Once both of your programs have terminated (regardless of what caused them to do
so), how many times did program 1 send a value?
-}

latterExample :: Program
latterExample = V.fromList
  [ SND $ Right 1
  , SND $ Right 2
  , SND $ Left 'p'
  , RCV 'a'
  , RCV 'b'
  , RCV 'c'
  , RCV 'd'
  ]

-- |
-- >>> part2 latterExample
-- 3
part2 :: Program -> Int
part2 = length . snd . dual

-- |
-- >>> dual latterExample
-- ([1,2,0],[1,2,1])
dual :: Program -> ([Int], [Int])
dual prog = (catMaybes xs, catMaybes ys) where
  xs = run (M.insert 'P' 0 $ M.singleton 'p' 0) 0 0 ys
  ys = run (M.insert 'P' 1 $ M.singleton 'p' 1) 0 0 xs

  -- batch them up by rcv
  --  x[0] - everything snd before the first rcv
  run :: M.Map Register Value -> Int -> Int -> [Maybe Int] -> [Maybe Int]
  run m !i !n zs
    | i < 0 || i >= V.length prog = []
    | otherwise = -- traceShow (i, prog V.! i, n, m) $
        let val (Left j) = fromMaybe 0 $ M.lookup j m
            val (Right v) = v
        in case prog V.! i of
          SND x   -> Just (val x) : run m (i + 1) (n + 1) zs
          SET x y -> run (M.insert x (val y) m) (i + 1) n zs
          ADD x y -> run (M.insertWith (+) x (val y) m) (i + 1) n zs
          MUL x y -> run (M.adjust (* val y) x m ) (i + 1) n zs
          MOD x y -> run (M.adjust (`rem` val y) x m) (i + 1) n zs
          RCV x   -> let go 0 (Nothing: ~[])  = []
                         go n (Just y:zs)     = run (M.insert x y m) (i + 1) n zs
                         go n (Nothing:zs)    = go (n - 1) zs
                         go _ []              = []
                      in Nothing : go n zs
          JGZ x y | val x <= 0 -> run m (i + 1) n zs
                  | otherwise  -> run m (i + val y) n zs
