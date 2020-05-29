{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Limp.External.SolutionSpec where

import           Test.Hspec
import           Test.Hspec.Expectations
import           Universum

import           Numeric.Limp.External.Solution (SolutionStatus (..), StopReason (..), solution)
import           Text.Parsec.String             (parseFromFile)

spec :: Spec
spec =
    describe "Solution parser" $ do
        let testCases =
                [ ("infeasible",      Infeasible)
                , ("timeout",         Stopped TimeLimit)
                , ("iterations",      Stopped IterationsLimit)
                , ("unbounded",       Unbounded)
                , ("xkcd-infeasible", Infeasible)
                , ("xkcd",            Optimal)
                ]
        forM_ testCases $ \(file, expectedStatus) ->
            it ("parses solution with status " <> show expectedStatus) $ do
                parseResult <- parseFromFile solution ("test/examples/" <> file <> ".sol")
                parseResult `shouldSatisfy` \case
                    (Right (actualStatus, _, _)) -> actualStatus == expectedStatus
                    _ -> False
