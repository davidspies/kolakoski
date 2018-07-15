module Term
  ( Term(..)
  , boolToTerm
  , termToBool
  , termToInt
  )
where

data Term = One | Two
  deriving (Eq, Show)

termToInt :: Term -> Int
termToInt = \case
  One -> 1
  Two -> 2

boolToTerm :: Bool -> Term
boolToTerm = \case
  False -> One
  True  -> Two

termToBool :: Term -> Bool
termToBool = \case
  One -> False
  Two -> True
