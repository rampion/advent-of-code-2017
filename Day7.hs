module Day7 where
import Data.Functor.Identity
import Control.Applicative 
import Text.Parsec hiding ((<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | -- >>> parseLine "llyhqfe (21)"
-- ("llyhqfe",21,[])
-- >>> parseLine "vpbdpfm (74) -> ndegtj, wnwxs"
-- ("vpbdpfm",74,["ndegtj","wnwxs"])
-- >>> parseLine "dosteiu (262) -> vliyv, rfxmk, nulxd, tckql"
-- ("dosteiu",262,["vliyv","rfxmk","nulxd","tckql"])
parseLine :: String -> (String, Int, [String])
parseLine xs = case runParser line () "<argument>" xs of
  Left e -> error (show e)
  Right t -> t

line :: Parsec String () (String, Int, [String])
line = do
  name <- many1 letter
  string " ("
  weight <- read <$> many1 digit
  string ")"
  names <- (string " -> " >> (many1 letter `sepBy1` string ", ")) <|> return []
  return (name, weight, names)

-- |
-- >>> part1 . map parseLine . lines <$> readFile "Day7.example"
-- "tknk"
part1 :: [(String, Int, [String])] -> String
part1 ts = head $ S.elems (srcs S.\\ dsts) where
  srcs = S.fromList [ src | (src, _, _) <- ts ]
  dsts = S.fromList [ dst | (_, _, ds) <- ts, dst <- ds ]

data Tower f = Tower { weight :: f Int, towers :: [Tower f] }

tower :: [(String, Int, [String])] -> Tower Identity
tower ts = m M.! root where
  m = M.fromList [ (n, Tower { weight = Identity w, towers = (m M.!) <$> ns }) | (n, w, ns) <- ts ]
  root = part1 ts

annotate :: Tower Identity -> Tower ((,) Int)
annotate (Tower (Identity w) ts) = Tower (w',w) ts' where
  ts' = annotate <$> ts
  w' = w + sum (totalWeight <$> ts')

totalWeight :: Tower ((,) Int) -> Int
totalWeight = fst . weight

findProblem :: Tower ((,) Int) -> Maybe Int
findProblem (Tower _ []) = Nothing
findProblem (Tower _ [t]) = findProblem t
findProblem (Tower _ [t,t']) = fixProblem (totalWeight t') t <|> fixProblem (totalWeight t) t'
findProblem (Tower _ ts) = case M.toList $ foldr (\t -> M.insertWith (++) (totalWeight t) [t]) M.empty ts of
  [(_,[t]),(w,_)] -> fixProblem w t
  [(w,_),(_,[t])] -> fixProblem w t
  _               -> Nothing

fixProblem :: Int -> Tower ((,) Int) -> Maybe Int
fixProblem w' (Tower (wa,w) ts) | w' == wa = Nothing
                                | otherwise = case filter (\t -> totalWeight t /= wb) ts of
    [t] -> fixProblem wb t
    _   -> Just $ w + (w' - wa)
  where
    (wb,r) = (w' - w) `quotRem` length ts

part2 :: [(String, Int, [String])] -> Int
part2 = fromMaybe 0 . findProblem . annotate . tower
