module Starts
  ( Starts
  , dropTermAt
  , fromList
  , initReverseInits
  , lengthExceeds
  , toList
  , zipToggle
  )
where

import           Data.MemoTrie                  ( HasTrie(..) )
import qualified Data.Bits                     as Bits
import           Test.QuickCheck                ( Arbitrary(..) )
import qualified Test.QuickCheck               as QuickCheck
import           Data.Bifunctor                 ( first )

import           BitList                        ( BitList )
import qualified BitList
import           Term                           ( Term )
import qualified Term

newtype Starts = Starts BitList
  deriving (Eq)

instance Show Starts where
  showsPrec d l = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (toList l)

instance Arbitrary Starts where
  arbitrary = Starts <$>
    QuickCheck.scale
      (floor . logBase 1.5 . (+ 1) . (fromIntegral :: Int -> Double))
      arbitrary

instance HasTrie Starts where
  newtype Starts :->: b = StartsTrie (BitList :->: b)
  trie f = StartsTrie $ trie (f . Starts)
  untrie (StartsTrie t) (Starts x) = untrie t x
  enumerate (StartsTrie t) = map (first Starts) (enumerate t)

fromList :: [Term] -> Starts
fromList = Starts . BitList.fromList . map Term.toBool

lengthExceeds :: Starts -> Int -> Bool
lengthExceeds (Starts xs) n = xs `BitList.lengthExceeds` n

initReverseInits :: Starts -> [Starts]
initReverseInits (Starts ts) = map Starts $ BitList.initReverseInits ts

dropTermAt :: Starts -> Int -> Starts
dropTermAt (Starts x) = Starts . Bits.clearBit x

toList :: Starts -> [Term]
toList (Starts x) = map Term.fromBool $ BitList.toList x

zipToggle :: Starts -> [Bool] -> Starts
zipToggle (Starts x) bs = Starts $ x `Bits.xor` BitList.fromList bs
