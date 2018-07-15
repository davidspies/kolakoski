module Starts
  ( Starts
  , fromList
  , inits
  , length
  , lengthExceeds
  , popFront
  , toList
  , zipToggle
  )
where

import           Prelude                 hiding ( length )

import           Data.MemoTrie                  ( HasTrie(..) )
import           Data.Bits                      ( xor )
import           Test.QuickCheck                ( Arbitrary(..) )
import qualified Test.QuickCheck               as QuickCheck
import           Data.Bifunctor                 ( first )

import           BitList                        ( BitList )
import qualified BitList
import           Term

newtype Starts = Starts BitList

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
fromList = Starts . BitList.fromList . map termToBool

length :: Starts -> Int
length (Starts xs) = BitList.length xs

lengthExceeds :: Starts -> Int -> Bool
lengthExceeds (Starts xs) n = xs `BitList.lengthExceeds` n

inits :: Starts -> [Starts]
inits (Starts ts) = map Starts $ BitList.inits ts

popFront :: Starts -> Maybe Starts
popFront (Starts ts) = Starts <$> BitList.popFront ts

toList :: Starts -> [Term]
toList (Starts x) = map boolToTerm $ BitList.toList x

zipToggle :: Starts -> [Bool] -> Starts
zipToggle (Starts x) bs = Starts $ x `xor` BitList.fromList bs
