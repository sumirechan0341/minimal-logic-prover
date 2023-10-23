module InputParser
    (
      parseInput,
      parseProblem
    ) where

import Text.ParserCombinators.Parsec
import Input
import CommandParser
import TacticParser
import Formula
import FormulaParser
import Text.ParserCombinators.Parsec.Char
import Data.Maybe
import qualified Data.Map.Lazy as M

type Context = M.Map String Formula
type Goal = [Formula]

input :: Parser Input
input = try (fmap TacticI tactic) <|> try (fmap CommandI command)

parseInput :: String -> Either ParseError Input
parseInput = parse input ""

context :: Parser (M.Map String Formula)
context = do
  let paren = between (char '{') (char '}')
  ctxM <- optionMaybe (paren (sepBy (spaces >> formula) (char ',')))
  let pairs = zip (map (("H" ++) . show) [0..]) (concat $ maybeToList ctxM)
  return $ M.fromList pairs

goal :: Parser Goal
goal =  sepBy (spaces >> formula) (char ',')

problem :: Parser (Context, Goal)
problem = do
  spaces
  ctx <- context
  spaces
  string "|-?"
  spaces
  g <- goal
  return (ctx, g)

parseProblem :: String -> Either ParseError (Context, Goal)
parseProblem = parse problem ""
