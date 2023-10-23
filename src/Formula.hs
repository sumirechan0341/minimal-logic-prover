module Formula
    (
      Formula(..)
    ) where

instance Show Formula where
  show (Arrow left right) = case left of
    (Arrow _ _) -> "(" ++ show left ++ ")" ++ " → " ++ show right
    _ -> show left ++ " → " ++ show right
  show (SimpleProp propName) = propName

data Formula = SimpleProp String | Arrow Formula Formula deriving (Eq)
