module Tactic
    (
      Tactic(..)
    ) where

data Tactic = Intro (Maybe String) | Intros | Apply String | Assumption | Exact String | ID deriving (Show, Eq)
