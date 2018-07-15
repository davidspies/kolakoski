module Lib
  ( Starts(..)
  , Term(..)
  , fromStarts
  , getKol
  , kolakoski
  , nStarts
  , startsLen
  , twos
  )
where

import           Control.Category               ( (>>>) )
import           Control.Parallel.Strategies
import           Data.List
import           Data.Maybe                     ( fromJust )
import           Data.MemoTrie
import           GHC.Generics                   ( Generic )
import qualified Test.QuickCheck               as QuickCheck
import           Test.QuickCheck                ( Arbitrary(..) )

data Term = One | Two
  deriving (Eq, Show, Generic)
newtype Starts = Starts [Term]
  deriving (Show, Generic)

instance Arbitrary Starts where
  arbitrary = Starts <$>
    QuickCheck.scale
      (floor . logBase 1.5 . (+ 1) . (fromIntegral :: Int -> Double))
      arbitrary

nStarts :: Starts -> Int
nStarts (Starts xs) = length xs

substarts :: Starts -> [Starts]
substarts (Starts ts) = map Starts (init (inits ts))

data CaseStarts = Empty | Single Term | Cons Term Starts

caseStarts :: Starts -> CaseStarts
caseStarts (Starts starts) = case starts of
  []       -> Empty
  [t     ] -> Single t
  (t : ts) -> Cons t (Starts ts)

instance HasTrie Starts where
  newtype Starts :->: b = StartsTrie { unStartsTrie :: Reg Starts :->: b }
  trie = trieGeneric StartsTrie
  untrie = untrieGeneric unStartsTrie
  enumerate = enumerateGeneric unStartsTrie

instance HasTrie Term where
  newtype Term :->: b = TermTrie { unTermTrie :: Reg Term :->: b }
  trie = trieGeneric TermTrie
  untrie = untrieGeneric unTermTrie
  enumerate = enumerateGeneric unTermTrie

instance Arbitrary Term where
  arbitrary = fmap boolToTerm arbitrary

opp :: Term -> Term
opp = \case
  One -> Two
  Two -> One

termToInt :: Term -> Int
termToInt = \case
  One -> 1
  Two -> 2

boolToTerm :: Bool -> Term
boolToTerm = \case
  False -> One
  True  -> Two

step :: [Term] -> Term -> [Term]
step current nextStart =
  concat $ zipWith (replicate . termToInt) current (cycleFrom nextStart)

cycleFrom :: Term -> [Term]
cycleFrom = \case
  One -> cycle [One, Two]
  Two -> cycle [Two, One]

fromStarts :: Starts -> [Term]
fromStarts (Starts starts) = case starts of
  []         -> []
  ts@(_ : _) -> foldl step [One] ts

startsLen :: Starts -> Integer
startsLen = memo go
 where
  go = caseStarts >>> \case
    Empty      -> 0
    Single _   -> 1
    Cons One r -> startsLen r
    Cons Two r ->
      let (fstgrp, sndgrp) =
            (startsLen r, startsLen (flipStarts r))
              `using` (if nStarts r < 30 then r0 else parTuple2 rseq rseq)
      in  fstgrp + sndgrp

flipStarts :: Starts -> Starts
flipStarts ts@(Starts ts') = Starts
  [ if odd count then opp t else t | (t, count) <- zip ts' counts ]
  where counts = map startsLen $ substarts ts

growRepeat :: a -> [[a]]
growRepeat x = res where res = [x] : map (x :) res

twos :: [Starts]
twos = map Starts $ growRepeat Two

getKol :: Integer -> Term
getKol n = case compare n 0 of
  LT -> error "Must be positive"
  EQ -> One
  GT -> posIn starts (n - 1)
   where
    starts =
      fromJust $ find ((> n - 1) . startsLen) (map Starts (growRepeat Two))

posIn :: Starts -> Integer -> Term
posIn starts n = case caseStarts starts of
  Empty                        -> error "empty starts"
  Single t | n == 0            -> t
  Single _                     -> error "n out of bounds"
  Cons One r                   -> posIn r n
  Cons Two r | n < startsLen r -> posIn r n
  Cons Two r                   -> posIn (flipStarts r) (n - startsLen r)

kolakoski :: [Term]
kolakoski = One : Two : xs
 where
  xs = Two : concat (zipWith (replicate . termToInt) xs (cycle [One, Two]))
