{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Numeric.Limp.External.MPS where

import           Universum          hiding (Constraint)

import           Numeric.Limp.Canon
import           Numeric.Limp.Rep

import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as Text

data ConstraintType = Eq | Gt | Lt

data MPSConstraint z r c =
    MPSConstraint
    { constraintType :: ConstraintType
    , function       :: Linear z r c
    , rhs            :: R c
    }

convertConstraint :: Eq (R c) => Constraint1 z r c -> [MPSConstraint z r c]
convertConstraint (C1 (Just lowerBound) formula (Just upperBound)) | lowerBound == upperBound = [MPSConstraint Eq formula lowerBound]
convertConstraint (C1 lowerBoundMb formula upperBoundMb) = catMaybes [MPSConstraint Gt formula <$> lowerBoundMb, MPSConstraint Lt formula <$> upperBoundMb]

convertConstraints :: Eq (R c) => Constraint z r c -> [MPSConstraint z r c]
convertConstraints (Constraint constraints) = mconcat $ convertConstraint <$> constraints

convert :: (Ord z, Ord r, Show r, Show z) => Program z r IntDouble -> Text
convert p = Text.unlines
    [ "NAME LIMP"
    , "ROWS"
    , rows
    , "COLUMNS"
    , columns
    , "RHS"
    , rhs
    , "BOUNDS"
    , bounds
    , "ENDATA"
    ]
    where
    variables = varsOfProgram p
    bounds = ""
    constraints = zip (("C"<>) . show <$> [1..]) $ convertConstraints (_constraints p)
    rows = Text.unlines $ " N OBJ":(mkRow <$> constraints)
    mkRow (ix, constraint) = " " <> tpe <> " " <> ix
        where
        tpe = case constraintType constraint of
            Eq -> "E"
            Lt -> "L"
            Gt -> "G"
    rhs = Text.unlines $ mkRhs <$> constraints
    mkRhs (ix, MPSConstraint _ _ value) = " RHS " <> ix <> " " <> show value
    columns = Text.unlines $ mkEntry <$> allEntries
    mkEntry (col, (row, value)) = " " <> col <> " " <> row <> " " <> value
    allEntries = sort $ objectiveEntries <> (mconcat $ (\(row, constraint) -> colEntries (row, function constraint)) <$> constraints)
    objectiveEntries = colEntries ("OBJ", _objective p)
    colEntries (row, Linear linear) = (\(col, value) -> (either show show col, (row, show value))) <$> Map.toList linear

    allBounds = uncurry mkBounds <$> Map.toList (_bounds p)
    mkBounds :: (Show z, Show r) => Either z r -> (Maybe (R IntDouble), Maybe (R IntDouble)) -> [Text]
    mkBounds variable (Just lowerBound, Just upperBound) | lowerBound == upperBound = [" FX BND1 " <> either show show variable <> " " <> show lowerBound]
    mkBounds variable@(Right _) (lowerBound, upperBound) = catMaybes $ [mkBound variable "LO" <$> lowerBound, mkBound variable "UP" <$> upperBound]
    mkBounds variable@(Left _) (Just 0.0, Just 1.0) = [mkBound variable "BV" 1.0]
    mkBounds variable@(Left _) (lowerBound, upperBound) = catMaybes $ [mkBound variable "LI" <$> lowerBound, mkBound variable "UI" <$> upperBound]
    mkBound :: (Show val, Show z, Show r) => Either z r  -> Text -> val -> Text
    mkBound variable tpe value = " " <> tpe <> " BND1 " <> either show show variable <> " " <> show value
