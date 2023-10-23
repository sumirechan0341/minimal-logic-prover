module TacticParser
    (
      tactic
    ) where

import Tactic
import Text.ParserCombinators.Parsec

intro :: Parser Tactic
intro = fmap Intro (spaces >> string "intro" >> skipMany1 space >> optionMaybe (many1 alphaNum))

intros :: Parser Tactic
intros = spaces >> string "intros" >> spaces >> return Intros

apply :: Parser Tactic
apply = fmap Apply (spaces >> string "apply" >> skipMany1 space >> many1 alphaNum)

assumption :: Parser Tactic
assumption = spaces >> string "assumption" >> spaces >> return Assumption

exact :: Parser Tactic
exact = fmap Exact (spaces >> string "exact" >> skipMany1 space >> many1 alphaNum)

idP :: Parser Tactic
idP = spaces >> string "id" >> spaces >> return ID

tactic :: Parser Tactic
tactic = try intro  <|> try intros <|> try apply <|> try assumption <|> try exact <|> try idP

parseTactic :: String -> Either ParseError Tactic
parseTactic = parse tactic ""
