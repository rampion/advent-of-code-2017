{-
--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to
simulate too many particles, and it won't be able to finish them all in time to
render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order
(starting with particle 0, then particle 1, particle 2, and so on). For each
particle, it provides the X, Y, and Z coordinates for the particle's position
(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties
are updated in the following order:

    Increase the X velocity by the X acceleration.
    Increase the Y velocity by the Y acceleration.
    Increase the Z velocity by the Z acceleration.
    Increase the X position by the X velocity.
    Increase the Y position by the Y velocity.
    Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would
like to know which particle will stay closest to position <0,0,0> in the long
term. Measure this using the Manhattan distance, which in this situation is
simply the sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay
entirely on the X-axis (for simplicity). Drawing the current states of
particles 0 and 1 (in that order) with an adjacent a number line and diagram of
current X positions (marked in parenthesis), the following would take place:

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and
so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?
-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-name-shadowing  #-}
module Main where
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Function (on)
import Control.Monad
import Data.Maybe (catMaybes, maybeToList)

main :: IO ()
main = do
  Right particles <- parseFromFile (particle3D `endBy` char '\n') "Day20.input"
  print . L.minimumBy (compare `on` (fmap magnitude . snd)) $ zip [0 :: Int ..] particles
  print $ remaining particles

data Particle a = Particle
  { acceleration :: a
  , velocity :: a
  , position :: a
  }
  deriving (Show, Ord, Eq, Functor)

magnitude :: (Int,Int,Int) -> Int
magnitude (x,y,z) = abs x + abs y + abs z

particle3D :: Parsec String () (Particle (Int,Int,Int))
particle3D = (\p v a -> Particle a v p) 
  <$> triple 'p' <* string ", "
  <*> triple 'v' <* string ", "
  <*> triple 'a'

triple :: Char -> Parsec String () (Int,Int,Int)
triple c = (,,) <$> (string (c : "=<") *> int) <*> (char ',' *> int) <*> (char ',' *> int) <* char '>'

int :: Parsec String () Int
int = option id (char '-' *> pure negate) <*> (read <$> many1 digit)

{-
To simplify the problem further, the GPU would like to remove any particles
that collide. Particles collide if their positions ever exactly match. Because
particles are updated simultaneously, more than two particles can collide at
the same time and place. Once particles collide, they are removed and cannot
collide with anything else after that tick.

For example:

p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)   
p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)      
p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

------destroyed by collision------    
------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
------destroyed by collision------                      (3)         
p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

In this example, particles 0, 1, and 2 are simultaneously destroyed at the time
and place marked X. On the next tick, particle 3 passes through unharmed.

How many particles are left after all collisions are resolved?
-}

type Tick = Int

example :: [Particle (Int,Int,Int)]
example = 
  [ Particle (0,0,0) (3,0,0) (-6,0,0)
  , Particle (0,0,0) (2,0,0) (-4,0,0)
  , Particle (0,0,0) (1,0,0) (-2,0,0)
  , Particle (0,0,0) (-1,0,0) (3,0,0)
  ]

-- |
-- >>> remaining example
-- 1
remaining :: [Particle (Int,Int,Int)] -> Int
remaining ps = length ps - S.size (annihilated ps) where

-- |
-- >>> annihilated example
-- fromList [0,1,2]
annihilated :: [Particle (Int,Int,Int)] -> S.Set Int
annihilated = L.foldl' step S.empty . fmap snd . M.toAscList . collisions where

  step rm = L.foldl' (flip $ \(i,j) -> S.insert i . S.insert j) rm
          . filter (\(i,j) -> not $ i `S.member` rm || j `S.member` rm)
  
-- |
-- >>> collisions example
-- fromList [(2,[(1,2),(0,2),(0,1)])]
collisions :: [Particle (Int,Int,Int)] -> M.Map Tick [(Int,Int)]
collisions ps = M.fromListWith (++) $ do
  (i,p):jqs <- L.tails $ zip [0 :: Int ..] ps
  (j,q) <- jqs
  t <- maybeToList $ collision p q
  return (t, [(i,j)])

-- |
-- >>> collision (example !! 0) (example !! 0)
-- Just 0
-- >>> collision (example !! 0) (example !! 1)
-- Just 2
-- >>> collision (example !! 0) (example !! 3)
-- Nothing
collision :: Particle (Int,Int,Int) -> Particle (Int,Int,Int) -> Maybe Tick
collision
  (Particle (ax0,ay0,az0) (vx0,vy0,vz0) (px0,py0,pz0))
  (Particle (ax1,ay1,az1) (vx1,vy1,vz1) (px1,py1,pz1))
  | null solutions = Just 0
  | otherwise      = fmap fst . S.minView $ L.foldl1' S.intersection solutions
  where solutions = catMaybes
          [ solve (ax0-ax1) (vx0-vx1) (px0-px1)
          , solve (ay0-ay1) (vy0-vy1) (py0-py1)
          , solve (az0-az1) (vz0-vz1) (pz0-pz1)
          ]
      
solve :: Int -> Int -> Int -> Maybe (S.Set Tick)
solve 0 0 0 = Nothing
solve 0 0 _ = Just $ S.empty
solve 0 vel pos = Just $ S.fromList [ q | let (q,r) = negate pos `quotRem` vel, q >= 0, r == 0 ]
solve acc vel pos = Just . S.fromList $ catMaybes [check (+), check (-)] where
  -- discrete acceleration:
  -- 0: acc | vel | pos
  -- 1: acc | acc + vel | acc + vel + pos
  -- 2: acc | 2*acc + vel | 3*acc + 2*vel + pos
  -- 3: acc | 3*acc + vel | 6*acc + 3*vel + pos
  -- ...
  -- t: acc | t*acc + vel | acc * t * (t + 1) / 2 + vel * t + pos
  -- 
  --  0 = acc * t * (t + 1) / 2 + vel * t + pos
  --    = acc * t * (t + 1) + 2 * vel * t + 2 * pos
  --    = acc * t ^ 2 + (2 * vel + acc) * t + 2 * pos
  a = acc
  b = 2 * vel + acc
  c = 2 * pos

  -- quadratic formula
  -- 0 = a*t^2 + b*t + c =>
  -- t = (-b +- sqrt(b^2 - 4ac)) / 2
  check op = do
    let discriminant = b*b - 4*a*c
    guard $ discriminant >= 0

    let d = round (sqrt $ fromIntegral discriminant :: Double)
    guard $ d * d == discriminant

    let (q,r) = (negate b `op` d) `quotRem` (2 * a)
    guard $ q >= 0 && r == 0

    return q
