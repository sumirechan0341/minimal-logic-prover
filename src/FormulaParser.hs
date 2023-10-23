module FormulaParser
    (
      formula
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Formula

parseFormula :: String -> Either ParseError Formula
parseFormula = parse formula ""

formula :: Parser Formula
formula = arrow <|> formula'

formula' :: Parser Formula
formula' = try paren <|> simpleProp

simpleProp :: Parser Formula
simpleProp = spaces >> (SimpleProp <$> many1 alphaNum) <* spaces

arrow :: Parser Formula
arrow = chainr1 formula' $ do
  spaces
  string "->"
  spaces
  return Arrow

paren :: Parser Formula
paren = do
  spaces
  char '('
  spaces
  body <- formula
  spaces
  char ')'
  spaces
  return body
