{-
--- Day 23: Coprocessor Conflagration ---

You decide to head directly to the CPU and fix the printer from there. As you
get close, you find an experimental coprocessor doing so much work that the
local programs are afraid it will halt and catch fire. This would cause serious
issues for the rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on
that tablet. The general functionality seems very similar, but some of the
instructions are different:

    set X Y sets register X to the value of Y.

    sub X Y decreases register X by the value of Y.

    mul X Y sets register X to the result of multiplying the value contained in
    register X by the value of Y.

    jnz X Y jumps with an offset of the value of Y, but only if the value of X
    is not zero. (An offset of 2 skips the next instruction, an offset of -1
    jumps to the previous instruction, and so on.)

    Only the instructions listed above are used. The eight registers here,
    named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for
testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul
instruction invoked?
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V'
import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  Right program <- P.parseFromFile programP "Day23.input"
  print $ part1 program
  print $ part2

programP :: P.Parsec String () Program
programP = V.fromList <$> (instructionP `P.endBy` P.newline)

instructionP :: P.Parsec String () Instruction
instructionP =  P.try (SET <$> (P.string "set " *> registerP) <*> (P.char ' ' *> operandP))
            <|> P.try (SUB <$> (P.string "sub " *> registerP) <*> (P.char ' ' *> operandP))
            <|> P.try (MUL <$> (P.string "mul " *> registerP) <*> (P.char ' ' *> operandP))
            <|> P.try (JNZ <$> (P.string "jnz " *> operandP) <*> (P.char ' ' *> operandP))

registerP :: P.Parsec String () Register
registerP = P.oneOf ['a'..'h']

operandP :: P.Parsec String () Operand
operandP = (Left <$> registerP) <|> (Right <$> integerP)

integerP :: P.Parsec String () Int
integerP = P.option id (negate <$ P.char '-') <*> (read <$> P.many1 P.digit)

data Instruction
  = SET Register Operand
  | SUB Register Operand
  | MUL Register Operand
  | JNZ Operand Operand

type Register = Char
type Operand = Either Register Int
type Program = V.Vector Instruction

part1 :: Program -> Int
part1 program = length . filter (isMUL . (program V.!) . fst) $ debug program M.empty

isMUL :: Instruction -> Bool
isMUL (MUL _ _) = True
isMUL _ = False

debug :: Program -> Registers -> [(Int, Registers)]
debug program initial
  = takeWhile (\(i,_) -> 0 <= i && i < n) 
  $ iterate (step program) (0, initial) where
    n = V.length program

-- |
-- >>> step (V.singleton $ SET 'a' (Right 5)) (0,  M.empty)
-- (1,fromList [('a',5)])
-- >>> step (V.singleton $ SET 'a' (Left 'b')) (0, M.singleton 'b' 7)
-- (1,fromList [('a',7),('b',7)])
step :: Program -> (Int, Registers) -> (Int, Registers)
step program (i,rs) = (i + d, adj rs) where
  val  = either (`getRegister` rs) id
  (adj, d) = case program V.! i of
    SET r (val -> v)          -> (M.insert r v, 1)
    SUB r (val -> v)          -> (M.adjust (subtract v) r, 1)
    MUL r (val -> v)          -> (M.adjust (*v) r, 1)
    JNZ (val -> v) (val -> d) -> (id, if v /= 0 then d else 1)

type Registers = M.Map Register Int

getRegister :: Register -> Registers -> Int
getRegister r = fromMaybe 0 . M.lookup r

{-
--- Part Two ---

Now, it's time to fix the problem.

The debug mode switch is wired directly to register a. You flip the switch,
which makes register a now start at 1 when the program is executed.

Immediately, the coprocessor begins to overheat. Whoever wrote this program
obviously didn't choose a very efficient implementation. You'll need to
optimize the program if it has any hope of completing before Santa needs that
printer working.

The coprocessor's ultimate goal is to determine the final value left in
register h once the program completes. Technically, if it had that... it
wouldn't even need to run the program.

After setting register a to 1, if the program were to run to completion, what
value would be left in register h?
-}
-- part2 :: Program -> Int
-- part2 = getRegister 'h' . snd . last . flip debug (M.singleton 'a' 1)

part2 :: Int
part2 = length $ filter isComposite [106500, 106500 + 17 .. 123500]

isComposite :: Int -> Bool
isComposite n = not $ primes V.! n

primes :: V.Vector Bool
primes = V.create $ do
    v <- V'.replicate 123501 True
    V'.write v 0 False
    V'.write v 1 False
    forM_ (takeWhile (\i -> i * i <= 123500) [2..]) $ \i -> do
      prime <- V'.read v i
      if not prime
        then return ()
        else forM_ ([i*i, i*i + i..123500]) $ V'.write v `flip` False
    return v
