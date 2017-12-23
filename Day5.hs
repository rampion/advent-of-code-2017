{-# LANGUAGE BangPatterns #-}
module Day5 where
import Control.Monad.ST
import Data.Vector
import qualified Data.Vector.Generic.Mutable as V

part1 :: [Int] -> Int
part1 as = runST (go 0 0 =<< thaw v) where
  v = fromList as
  n = Data.Vector.length v
  go :: Int -> Int -> MVector s Int -> ST s Int
  go !jumps !pos v
    | pos < 0 || n <= pos = return jumps
    | otherwise = do
        a <- V.read v pos
        V.write v pos $! a + 1
        go (jumps + 1) (pos + a) v

part2 :: [Int] -> Int
part2 as = runST (go 0 0 =<< thaw v) where
  v = fromList as
  n = Data.Vector.length v
  go :: Int -> Int -> MVector s Int -> ST s Int
  go !jumps !pos v
    | pos < 0 || n <= pos = return jumps
    | otherwise = do
        a <- V.read v pos
        V.write v pos $! (if a >= 3 then a - 1 else a + 1)
        go (jumps + 1) (pos + a) v
