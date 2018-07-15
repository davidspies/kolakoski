module Lib
  ( fromStarts
  , getKol
  , kolakoski
  , startsLen
  , twos
  )
where

import           Control.Category               ( (>>>) )
import           Control.Parallel.Strategies
import           Data.List
import           Data.Maybe                     ( fromJust )
import           Data.MemoTrie

import           Term
import           Starts

step :: [Term] -> Term -> [Term]
step current nextStart =
  concat $ zipWith (replicate . termToInt) current (cycleFrom nextStart)

cycleFrom :: Term -> [Term]
cycleFrom = \case
  One -> cycle [One, Two]
  Two -> cycle [Two, One]

kolakoski :: [Term]
kolakoski = One : Two : xs
 where
  xs = Two : concat (zipWith (replicate . termToInt) xs (cycle [One, Two]))

fromStarts :: Starts -> [Term]
fromStarts = Starts.toList >>> \case
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
              `using` (if parallelize r then parTuple2 rseq rseq else r0)
      in  fstgrp + sndgrp

parallelize :: Starts -> Bool
parallelize = (`Starts.lengthExceeds` 30)

flipStarts :: Starts -> Starts
flipStarts ts =
  let whichOdds = map (odd . startsLen) (init $ Starts.inits ts)
  in  zipToggle ts whichOdds

growRepeat :: a -> [[a]]
growRepeat x = res where res = [x] : map (x :) res

twos :: [Starts]
twos = map Starts.fromList $ growRepeat Two

getKol :: Integer -> Term
getKol n = case compare n 0 of
  LT -> error "Must be non-negative"
  EQ -> One
  GT -> posIn starts (n - 1)
    where starts = fromJust $ find ((n - 1 <) . startsLen) twos

posIn :: Starts -> Integer -> Term
posIn starts n = case caseStarts starts of
  Empty                        -> error "empty starts"
  Single t | n == 0            -> t
  Single _                     -> error "n out of bounds"
  Cons One r                   -> posIn r n
  Cons Two r | n < startsLen r -> posIn r n
  Cons Two r                   -> posIn (flipStarts r) (n - startsLen r)
