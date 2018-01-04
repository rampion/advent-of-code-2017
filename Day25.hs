{-
--- Day 25: The Halting Problem ---

Following the twisty passageways deeper and deeper into the CPU, you finally
reach the core of the computer. Here, in the expansive central chamber, you
find a grand apparatus that fills the entire room, suspended nanometers above
your head.

You had always imagined CPUs to be noisy, chaotic places, bustling with
activity. Instead, the room is quiet, motionless, and dark.

Suddenly, you and the CPU's garbage collector startle each other. "It's not
often we get many visitors here!", he says. You inquire about the stopped
machinery.

"It stopped milliseconds ago; not sure why. I'm a garbage collector, not a
doctor." You ask what the machine is for.

"Programs these days, don't know their origins. That's the Turing machine! It's
what makes the whole computer work." You try to explain that Turing machines
are merely models of computation, but he cuts you off. "No, see, that's just
what they want you to think. Ultimately, inside every CPU, there's a Turing
machine driving the whole thing! Too bad this one's broken. We're doomed!"

You ask how you can help. "Well, unfortunately, the only way to get the
computer running again would be to create a whole new Turing machine from
scratch, but there's no way you can-" He notices the look on your face, gives
you a curious glance, shrugs, and goes back to sweeping the floor.

You find the Turing machine blueprints (your puzzle input) on a tablet in a
nearby pile of debris. Looking back up at the broken Turing machine above, you
can start to identify its parts:

    A tape which contains 0 repeated infinitely to the left and right.

    A cursor, which can move left or right along the tape and read or write
    values at its current position.

    A set of states, each containing rules about what to do based on the
    current value under the cursor.

Each slot on the tape has two possible values: 0 (the starting value for all
slots) and 1. Based on whether the cursor is pointing at a 0 or a 1, the
current state says what value to write at the current position of the cursor,
whether to move the cursor left or right one slot, and which state to use next.

For example, suppose you found the following blueprint:

Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.

Running it until the number of steps required to take the listed diagnostic
checksum would result in the following tape configurations (with the cursor
marked in square brackets):

... 0  0  0 [0] 0  0 ... (before any steps; about to run state A)
... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B)
... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A)
... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B)
... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A)
... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B)
... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A)

The CPU can confirm that the Turing machine is working by taking a diagnostic
checksum after a specific number of steps (given in the blueprint). Once the
specified number of steps have been executed, the Turing machine should pause;
once it does, count the number of times 1 appears on the tape. In the above
example, the diagnostic checksum is 3.

Recreate the Turing machine and save the computer! What is the diagnostic
checksum it produces once it's working again?
-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  Right (startingState, numSteps, table) <- P.parseFromFile inputP "Day25.input"
  print $ part1 startingState numSteps table

-- |
-- >>> P.parseFromFile inputP "Day25.example"
-- Right ('A',6,fromList [('A',((True,Right (),'B'),(False,Left (),'B'))),('B',((True,Left (),'A'),(True,Right (),'A')))])
inputP :: P.Parsec String () (State, Int, StateTransitionTable)
inputP = (,,)
  <$> startingStateP <* P.newline
  <*> checksumStepsP <* P.newline
  <*> (M.fromList <$> (stateTransitionsP `P.endBy` P.newline))
  <* P.eof

startingStateP :: P.Parsec String () State
startingStateP = P.string "Begin in state " *> stateP <* P.string "."

stateP :: P.Parsec String () State
stateP = P.oneOf ['A'..'Z']

bitP :: P.Parsec String () Bit
bitP = ('1'==) <$> P.oneOf "01"

checksumStepsP :: P.Parsec String () Int
checksumStepsP = read <$> (P.string "Perform a diagnostic checksum after " *> P.many1 P.digit <* P.string " steps.")

stateTransitionsP :: P.Parsec String () (State,(Transition,Transition))
stateTransitionsP = do
  _     <- P.newline
  state <- P.string "In state " *> stateP <* P.string ":\n"
  false <- transitionP False
  _     <- P.newline
  true <- transitionP True
  return (state,(false,true))

transitionP :: Bool -> P.Parsec String () Transition
transitionP b = do
  bit   <- P.string "  If the current value is " *> bitP <* P.string ":\n"
  guard $ bit == b
  value <- P.string "    - Write the value " *> bitP <* P.string ".\n"
  dir   <- P.string "    - Move one slot to the " *> dirP <* P.string ".\n"
  next  <- P.string "    - Continue with state " *> stateP <* P.string "."
  return $ (value,dir,next)

dirP :: P.Parsec String () Direction
dirP = Left () <$ P.string "left" <|> Right () <$ P.string "right"

type Transition = (Bit,Direction,State)
type StateTransitionTable = M.Map State (Transition,Transition)
type State = Char
type Bit = Bool
type Direction = Either () ()

-- |
-- >>> Right (startingState, numSteps, table) <- P.parseFromFile inputP "Day25.example"
-- >>> part1 startingState numSteps table
-- 3
part1 :: State -> Int -> StateTransitionTable -> Int
part1 startingState numSteps table = checksum . snd 
  $ iterate (uncurry $ step table) (startingState, blankTape) !! numSteps

-- |
-- >>> checksum blankTape
-- 0
checksum :: Tape -> Int
checksum (Tape {..}) = sum (fromEnum <$> fore) + fromEnum here + sum (fromEnum <$> back)

step :: StateTransitionTable -> State -> Tape -> (State, Tape)
step table state tape@(Tape {..}) = (state',tape') where
  (here',dir,state') = (if here then snd else fst) (table M.! state)
  tape' = go dir tape { here = here' }

go :: Direction -> Tape -> Tape
go (Right ()) (Tape {..}) = Tape back' here' fore' where
  back' = push here back
  (here', fore') = pop fore
go (Left ()) (Tape {..}) = Tape back' here' fore' where
  (here', back') = pop back
  fore' = push here fore
  
push :: Bool -> [Bool] -> [Bool]
push False [] = []
push b bs = b:bs

pop :: [Bool] -> (Bool,[Bool])
pop [] = (False, [])
pop (b:bs) = (b, bs)

blankTape :: Tape
blankTape = Tape [] False []

data Tape = Tape
  { back :: [Bool]
  , here :: Bool
  , fore :: [Bool]
  }

