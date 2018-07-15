module Term where

data Term = One | Two
  deriving (Eq, Show)

toInt :: Term -> Int
toInt = \case
  One -> 1
  Two -> 2

fromBool :: Bool -> Term
fromBool = \case
  False -> One
  True  -> Two

toBool :: Term -> Bool
toBool = \case
  One -> False
  Two -> True
