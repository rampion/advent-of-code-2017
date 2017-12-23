module Day4 where
import Data.Map

type Signature = Map Char Int

signature :: String -> Signature
signature = Prelude.foldr (insertWith (+) `flip` 1) empty

-- |
-- >>> isValid "abcde fghij"
-- True
-- >>> isValid "abcde xyz ecdab"
-- False
-- >>> isValid "a ab abc abd abf abj"
-- True
-- >>> isValid "iiii oiii ooii oooi oooo"
-- True
-- >>> isValid "oiii ioii iioi iiio"
-- False
--
isValid :: String -> Bool
isValid = all (==1) . elems . Prelude.foldr (insertWith (+) `flip` 1) empty . fmap signature . words
