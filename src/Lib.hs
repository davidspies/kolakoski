module Lib
  ( fromStarts
  , getKol
  , kolakoski
  , startsLen
  , twos
  )
where

import           Control.Parallel.Strategies
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )
import           Data.MemoTrie                  ( memo )

import qualified Term
import           Term                           ( Term(One, Two) )
import qualified Starts
import           Starts                         ( Starts )

step :: [Term] -> Term -> [Term]
step current nextStart =
  concat $ zipWith (replicate . Term.toInt) current (cycleFrom nextStart)

cycleFrom :: Term -> [Term]
cycleFrom = \case
  One -> cycle [One, Two]
  Two -> cycle [Two, One]

kolakoski :: [Term]
kolakoski = One : Two : xs
 where
  xs = Two : concat (zipWith (replicate . Term.toInt) xs (cycle [One, Two]))

fromStarts :: Starts -> [Term]
fromStarts = foldl step [One] . Starts.toList

data CaseStarts = Single Term | Multi Starts Starts
  deriving (Show)

breakDown :: Starts -> CaseStarts
breakDown starts = case Starts.popFront starts of
  Nothing -> Single One
  Just r | starts `Starts.lengthExceeds` 1 -> Multi
    r
    (Starts.zipToggle r whichOdds)
   where
    whichOdds = map (odd . startsLen . fromJust . Starts.popFront)
                    (init $ tail $ Starts.inits starts)
  Just _ -> Single Two

startsLen :: Starts -> Integer
startsLen = memo go
 where
  go x = case breakDown x of
    Single _ -> 1
    Multi fstgrp sndgrp ->
      let (fstlen, sndlen) =
            (startsLen fstgrp, startsLen sndgrp)
              `using` (if parallelize x then parTuple2 rseq rseq else r0)
      in  max 1 (fstlen + sndlen)

parallelize :: Starts -> Bool
parallelize = (`Starts.lengthExceeds` 30)

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
posIn starts n = case breakDown starts of
  Single t | n == 0                     -> t
  Single _                              -> error "n out of bounds"
  Multi fstgrp _ | n < startsLen fstgrp -> posIn fstgrp n
  Multi fstgrp sndgrp                   -> posIn sndgrp (n - startsLen fstgrp)
