module CommandParser
    (
      command
    ) where

import Text.ParserCombinators.Parsec
import Command


logP :: Parser Command
logP = fmap Log $ spaces >> string "log" >> spaces >> many1 alphaNum

undo :: Parser Command
undo = spaces >> string "undo" >> return Undo

quit :: Parser Command
quit = spaces >> (string "quit" <|> string ":q") >> return Quit

command :: Parser Command
command = try logP <|> try undo <|> quit

parseCommand :: String -> Either ParseError Command
parseCommand = parse command ""
