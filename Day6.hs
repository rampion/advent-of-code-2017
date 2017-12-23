module Day6 where
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (forM_)
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Maybe (catMaybes)

-- |
-- >>> redistribute $ V.fromList [0,2,7,0]
-- [2,4,1,2]
-- >>> redistribute $ V.fromList [2,4,1,2]
-- [3,1,2,3]
-- >>> redistribute $ V.fromList [3,1,2,3]
-- [0,2,3,4]
-- >>> redistribute $ V.fromList [0,2,3,4]
-- [1,3,4,1]
-- >>> redistribute $ V.fromList [1,3,4,1]
-- [2,4,1,2]
redistribute :: V.Vector Int -> V.Vector Int
redistribute v = runST $ do
  let n = V.length v
      i = V.maxIndex v
  v <- V.thaw v
  a <- MV.read v i
  let (q,r) = a `quotRem` n
  forM_ [i + 1 .. i + r]          $ MV.modify v (+(q+1)) . (`rem` n)
  forM_ [i + r + 1 .. i + n - 1]  $ MV.modify v (+q) . (`rem` n)
  MV.write v i q
  V.freeze v

-- |
-- >>> step1 [0,2,7,0]
-- 5
step1 :: [Int] -> Int
step1 as = maybe 0 id $ L.findIndex id (zipWith S.member vs ks) where
  vs = iterate redistribute (V.fromList as)
  ks = L.scanl' (flip S.insert) S.empty vs

-- |
-- >>> step2 [0,2,7,0]
-- 4
step2 :: [Int] -> Int
step2 as = head . catMaybes $ zipWith3 (\i v k -> (i-) <$> M.lookup v k) [0..] vs ks where
  vs = iterate redistribute (V.fromList as)
  ks = L.scanl' (\m (i,v) -> M.insert v i m) M.empty $ zip [0..] vs
