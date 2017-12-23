{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Word128 (Word128()) where
import Data.Word
import Data.Bits
import qualified Data.List as L
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Positive(..))
import GHC.Generics

data Word128 = Word128 !Word64 !Word64
  deriving (Eq, Ord, Generic)

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary
  shrink (Word128 hi lo) = [ Word128 hi' lo | hi' <- shrink hi ] ++ [ Word128 hi lo' | lo' <- shrink lo ]

instance CoArbitrary Word128

-- |
-- prop> (w :: Word128) .&. w' == fromIntegral (toInteger w .&. toInteger w')
-- prop> (w :: Word128) .|. w' == fromIntegral (toInteger w .|. toInteger w')
-- prop> (w :: Word128) `xor` w' == fromIntegral (toInteger w `xor` toInteger w')
-- prop> (w :: Word128) `shift` i == fromIntegral (toInteger w `shift` i)
instance Bits Word128 where
  Word128 ah al .&. Word128 bh bl = Word128 (ah .&. bh) (al .&. bl)
  Word128 ah al .|. Word128 bh bl = Word128 (ah .|. bh) (al .|. bl)
  Word128 ah al `xor` Word128 bh bl = Word128 (ah `xor` bh) (al `xor` bl)
  complement (Word128 hi lo) = Word128 (complement hi) (complement lo)
  zeroBits = Word128 zeroBits zeroBits
  bit i | i >= 64   = Word128 (bit $ i - 64) 0
        | otherwise = Word128 0 (bit i)
  Word128 hi lo `setBit` i
    | i >= 64   = Word128 (setBit hi $ i - 64) lo
    | otherwise = Word128 hi (setBit lo i)
  Word128 hi lo `clearBit` i
    | i >= 64   = Word128 (clearBit hi $ i - 64) lo
    | otherwise = Word128 hi (clearBit lo i)
  Word128 hi lo `testBit` i 
    | i >= 64   = testBit hi (i - 64)
    | otherwise = testBit lo i
  bitSizeMaybe = Just . finiteBitSize
  bitSize = finiteBitSize
  isSigned = const False
  popCount (Word128 hi lo) = popCount hi + popCount lo
  Word128 hi lo `shiftL` i =
    Word128 ((hi `shiftL` i) .|. (lo `shift` (i - 64))) (lo `shiftL` i)
  Word128 hi lo `shiftR` i =
    Word128 (hi `shiftR` i) ((hi `shift` (64 - i)) .|. (lo `shiftR` i))
  Word128 hi lo `rotateL` i =
    Word128 ((hi `shiftL` j) .|. (lo `shift` (j - 64)) .|. (hi `shiftR` (128 - j)))
            ((lo `shiftL` j) .|. (hi `shift` (64 - j)) .|. (lo `shiftR` (128 - j)))
    where j = i `rem` 128
  w `rotateR` i = w `rotateL` (128 - i)

instance FiniteBits Word128 where
  finiteBitSize _ = 128
  countLeadingZeros (Word128 hi lo) | n == 64   = n + countLeadingZeros lo
                                    | otherwise = n
    where n = countLeadingZeros hi
  countTrailingZeros (Word128 hi lo)  | n == 64   = n + countTrailingZeros hi
                                      | otherwise = n
    where n = countTrailingZeros lo

-- |
-- prop> prop_quotRem
prop_quotRem :: Word128 -> Positive Word128 -> Bool
prop_quotRem a (Positive b) = a `quotRem` b == (fromIntegral q, fromIntegral r)
  where (q,r) = toInteger a `quotRem` toInteger b

instance Integral Word128 where
  toInteger (Word128 q r) = 
    (toInteger q `shiftL` 64) .|. toInteger r
  quotRem = divide

divide :: (Ord b, Num b, FiniteBits b) => b -> b -> (b, b)
divide n d = L.foldl' step (0, 0) [finiteBitSize d - 1, finiteBitSize d - 2 .. 0] where
  step (!q,!r) i =
    let r' = (r `shiftL` 1) .|. (if n `testBit` i then 1 else 0)
    in if r' >= d then (setBit q i, r' - d) else (q, r')

instance Real Word128 where
  toRational = toRational . toInteger

-- |
-- prop> split (join (fromIntegral (x :: Word32)) (fromIntegral (y :: Word32))) == (fromIntegral x,fromIntegral y)
split :: Word64 -> (Word64, Word64)
split = flip quotRem $ 1 `shiftL` 32

-- |
-- prop> uncurry join (split x) == x
join :: Word64 -> Word64 -> Word64
join hi lo = (hi `shiftL` 32) + lo

-- |
-- prop> (w :: Word128) + w' == fromIntegral (toInteger w + toInteger w')
-- prop> negate (w :: Word128) == fromIntegral (negate (toInteger w))
-- prop> (w :: Word128) - w' == fromIntegral (toInteger w - toInteger w')
-- prop> (w :: Word128) * w' == fromIntegral (toInteger w * toInteger w')
instance Num Word128 where
  Word128 ah al + Word128 bh bl = 
    Word128 (ah + bh + ch) (join cl dl) where

    (alh, all) = split al
    (blh, bll) = split bl

    (ch, cl) = split $ alh + blh + dh
    (dh, dl) = split $ all + bll

  Word128 ah al * Word128 bh bl =
    Word128 
      (ah * bl + al * bh + alh * blh + ch + dh + fh) 
      (join fl el) where 

    (alh, all) = split al
    (blh, bll) = split bl

    (ch,cl) = split $ alh * bll
    (dh,dl) = split $ all * blh
    (eh,el) = split $ all * bll
    (fh,fl) = split $ cl + dl + eh

  negate b = complement b + 1

  abs = id
  signum = const 1
  fromInteger i = Word128 q r where
    q = fromInteger $ i `shiftR` 64
    r = fromInteger $ i .&. (shiftL 1 64 - 1)

instance Show Word128 where
  showsPrec p = showsPrec p . toInteger

instance Enum Word128 where
  toEnum i = Word128 q r where
    q = toEnum $ i `shiftR` 64
    r = toEnum $ i .&. (shiftL 1 64 - 1)
  fromEnum (Word128 q r) = 
    (fromEnum q `shiftL` 64) .|. fromEnum r

