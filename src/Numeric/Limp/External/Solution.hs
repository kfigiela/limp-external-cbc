{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Limp.External.Solution where

import           Universum                              hiding (many, try)

import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Text.Parsec                            hiding ((<|>))
import           Text.Parsec                            ((<?>))
import           Text.Parsec.Char                       (char, digit, letter, spaces, string)
import           Text.Parsec.String                     (Parser)
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import           Text.ParserCombinators.Parsec.Number   (floating2, sign)
import qualified Text.ParserCombinators.Parsec.Token    as P

data SolutionStatus = Optimal | Infeasible | Unbounded | Stopped StopReason | Unknown
  deriving (Show)

data StopReason = TimeLimit | IterationsLimit | Difficulties | Interrupt
  deriving (Show)

solutionStatus :: Parser SolutionStatus
solutionStatus =
  Optimal <$ string "Optimal"
    <|> Infeasible <$ string "Infeasible"
    <|> Infeasible <$ string "Integer infeasible"
    <|> Unbounded <$ string "Unbounded"
    <|> Stopped TimeLimit <$ string "Stopped on time"
    <|> Stopped IterationsLimit <$ string "Stopped on iterations"
    <|> Stopped Difficulties <$ string "Stopped on difficulties"
    <|> Stopped Interrupt <$ string "Stopped on ctrl-c"
    <|> Unknown <$ string "Status unknown"

objectiveValue :: Parser Double
objectiveValue = string " - objective value " *> signedFloating

values :: Parser (Map Text (Bool, Double, Double))
values = Map.fromList . catMaybes <$> (Just <$> value <|> Nothing <$ string "") `sepBy` newline

value :: Parser (Text, (Bool, Double, Double))
value =
  (\status varName value dual -> (varName, (status, value, dual)))
    <$> (status <* skipIndex)
    <*> (toText <$> varName <* spaces)
    <*> (signedFloating <* spaces)
    <*> signedFloating
  where
    skipIndex = many1 digit <* spaces
    status = option True (False <$ (char '*' >> char '*')) <* spaces
    varName = (:) <$> letter <*> many (letter <|> digit <|> oneOf "_[]{}/.&#$%~'@^")

solution :: Parser (SolutionStatus, Double, Map Text (Bool, Double, Double))
solution = (,,) <$> solutionStatus <*> (objectiveValue <* newline) <*> (values <* eof)

signedFloating :: Parser Double
signedFloating = option id sign <*> floating2 False
