module Input
    (
      Input(..)
    ) where

import Tactic
import Command

data Input = TacticI Tactic | CommandI Command
