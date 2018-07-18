module BitList
  ( BitList
  , empty
  , fromList
  , lengthExceeds
  , initReverseInits
  , toList
  )
where

import           Data.List                      ( foldl' )
import           Data.Bits
import           Data.MemoTrie                  ( HasTrie(..) )
import           Test.QuickCheck                ( Arbitrary(..) )

newtype BitList = BitList {unBL :: Integer}
  deriving (Eq, Bits)

instance Show BitList where
  showsPrec d l = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (toList l)

instance Arbitrary BitList where
  arbitrary = fromList <$> arbitrary

instance HasTrie BitList where
  newtype BitList :->: b = BitListTrie (Integer :->: b)
  trie f = BitListTrie $ trie (f . BitList)
  untrie (BitListTrie t) (BitList x) = untrie t x
  enumerate (BitListTrie t) = [(BitList n, untrie t n) | n <- [0..]]

empty :: BitList
empty = BitList 0

fromList :: [Bool] -> BitList
fromList =
  foldl' (\i b -> (if b then (`setBit` 0) else id) (i `shiftL` 1)) zeroBits

toList :: BitList -> [Bool]
toList = go [] . unBL
 where
  go :: [Bool] -> Integer -> [Bool]
  go accum = \case
    0 -> accum
    n -> go (testBit n 0 : accum) (n `shiftR` 1)

lengthExceeds :: BitList -> Int -> Bool
lengthExceeds (BitList x) k = x >= bit k

initReverseInits :: BitList -> [BitList]
initReverseInits = takeWhile (/= zeroBits) . iterate (`shiftR` 1)
