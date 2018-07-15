{-# LANGUAGE MagicHash #-}

module BitList
  ( BitList
  , empty
  , fromList
  , inits
  , length
  , lengthExceeds
  , popFront
  , toList
  )
where

import           Prelude                 hiding ( length )

import           Data.List                      ( foldl' )
import           Data.Bits
import           Data.MemoTrie                  ( HasTrie(..) )
import           GHC.Exts                       ( Word(W#) )
import           GHC.Integer.GMP.Internals      ( sizeInBaseInteger )
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
  enumerate (BitListTrie t) = [(BitList n, untrie t n) | n <- [1..]]

empty :: BitList
empty = BitList 0

fromList :: [Bool] -> BitList
fromList = BitList . foldl' (\i b -> (i `shiftL` 1) .|. (if b then 1 else 0)) 0

toList :: BitList -> [Bool]
toList = go [] . unBL
 where
  go :: [Bool] -> Integer -> [Bool]
  go accum = \case
    0 -> accum
    n -> go (testBit n 0 : accum) (n `shiftR` 1)

length :: BitList -> Int
length (BitList x) | x == 0    = 0
                   | otherwise = fromIntegral (W# (sizeInBaseInteger x 2#))

lengthExceeds :: BitList -> Int -> Bool
lengthExceeds (BitList x) k = x >= bit k

popFront :: BitList -> Maybe BitList
popFront bl@(BitList x) = case length bl of
  0 -> Nothing
  n -> Just $ BitList $ x `clearBit` (n - 1)

inits :: BitList -> [BitList]
inits (BitList x) =
  reverse $ map BitList $ takeWhile (> 0) $ iterate (`shiftR` 1) x
