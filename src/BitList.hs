{-# LANGUAGE MagicHash #-}

module BitList
  ( BitList
  , empty
  , fromList
  , inits
  , length
  , lengthAtMost
  , null
  , popFront
  , toList
  , xorList
  )
where

import           Prelude                 hiding ( length
                                                , null
                                                )

import           Data.List                      ( foldl' )
import           Data.Bits
import           Data.MemoTrie
import           GHC.Exts                       ( Word(W#) )
import           GHC.Integer.GMP.Internals      ( sizeInBaseInteger )
import           Test.QuickCheck                ( Arbitrary(..) )

newtype BitList = BitList {unBL :: Integer}

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
empty = BitList 1

appendBits :: Integer -> [Bool] -> Integer
appendBits = foldl' (\i b -> (i `shiftL` 1) .|. (if b then 1 else 0))

fromList :: [Bool] -> BitList
fromList = BitList . (1 `appendBits`)

toList :: BitList -> [Bool]
toList = go [] . unBL
 where
  go :: [Bool] -> Integer -> [Bool]
  go accum = \case
    1 -> accum
    n -> go (testBit n 0 : accum) (n `shiftR` 1)

-- Length of rhs must be at most length of lhs. Precondition is not checked.
xorList :: BitList -> [Bool] -> BitList
xorList (BitList x) = BitList . (x `xor`) . (0 `appendBits`)

length :: BitList -> Int
length (BitList x) = fromIntegral (W# (sizeInBaseInteger x 2#)) - 1

null :: BitList -> Bool
null (BitList x) = x <= 1

lengthAtMost :: BitList -> Int -> Bool
lengthAtMost (BitList x) k = x < bit k

popFront :: BitList -> Maybe (Bool, BitList)
popFront bl@(BitList x) = case length bl of
  0 -> Nothing
  n ->
    let b = x `testBit` (n - 1)
    in  Just (b, BitList $ x `clearBit` n `setBit` (n - 1))

inits :: BitList -> [BitList]
inits (BitList x) =
  reverse $ map BitList $ takeWhile (> 0) $ iterate (`shiftR` 1) x
