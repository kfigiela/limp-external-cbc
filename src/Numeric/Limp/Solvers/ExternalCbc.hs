{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Limp.Solvers.ExternalCbc where

import           Universum

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Numeric.Limp.Canon             (Program, varsOfProgram)
import           Numeric.Limp.External.LP       (NamedVariable, convert, varName)
import           Numeric.Limp.External.Solution (SolutionStatus, solution)
import           Numeric.Limp.Rep               (Assignment (..), IntDouble, R (..), Z (..))
import           System.Exit                    (ExitCode)
import           System.IO.Temp                 (withSystemTempDirectory)
import           System.Process                 (rawSystem)
import           Text.Parsec.String             (parseFromFile)

type Error = Text

cbcArgs :: FilePath -> FilePath -> [String]
cbcArgs problemFile solutionFile =
  [ problemFile,
    "statistics",
    "printingOptions=all",
    "solve",
    "solution",
    solutionFile
  ]

type ObjectiveValue = Double

solve :: (Ord z, Ord r, NamedVariable z, NamedVariable r) => Program z r IntDouble -> IO (Either Error (SolutionStatus, ObjectiveValue, Assignment z r IntDouble))
solve p =
  withSystemTempDirectory "limp.lp" $ \tmpDir -> do
    let problemFile = tmpDir <> "/problem.lp"
        solutionFile = tmpDir <> "/problem.solution"
        lpProgram = convert p

    writeFile problemFile lpProgram
    status <- rawSystem "cbc" $ cbcArgs problemFile solutionFile
    result <- readFile solutionFile
    parsedSolution <- parseFromFile solution solutionFile
    pure $ case parsedSolution of
      Left err                            -> Left $ "Parse error: " <> show err
      Right (status, objective, mappings) -> Right (status, objective, getSolution p mappings)

getSolution :: (NamedVariable z, NamedVariable r, Ord z, Ord r) => Program z r IntDouble -> Map Text (Bool, Double, Double) -> Assignment z r IntDouble
getSolution p mappings = Assignment zMap rMap
  where
    allVars = Set.toList (varsOfProgram p)
    zVars = lefts allVars
    rVars = rights allVars
    varValues = (\(_, v, _) -> v) <$> mappings
    zMap = Map.fromList $ (\var -> (var, Z $ round $ Map.findWithDefault 0 (varName var) varValues)) <$> zVars
    rMap = Map.fromList $ (\var -> (var, R $ Map.findWithDefault 0.0 (varName var) varValues)) <$> rVars
