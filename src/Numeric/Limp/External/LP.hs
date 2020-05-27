-- http://lpsolve.sourceforge.net/5.0/CPLEX-format.htm
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Limp.External.LP where

import           Universum          hiding (Constraint)

import           Numeric.Limp.Canon
import           Numeric.Limp.Rep

import           Data.Char          (isDigit, isLetter)
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

class NamedVariable var where
    varName :: var -> Text
    default varName :: Show var => var -> Text
    varName = show

instance (NamedVariable z, NamedVariable r) => NamedVariable (Either z r) where
    varName = either varName varName
instance NamedVariable Text where
    varName = filterChars
instance NamedVariable String where
    varName = filterChars . toText

filterChars :: Text -> Text
filterChars str = case Text.uncons filtered of
        Just (c, _t) | isLetter c -> filtered
        _                         -> "v" <> filtered
    where
        f :: Char -> Bool
        f c = isLetter c || isDigit c || c `elem` ("_[]{}/.&#$%~'@^" :: String)
        filtered = Text.filter f str

linearFunction :: (NamedVariable z, NamedVariable r, Show (R c), Show (Z c)) => Linear z r c -> Text
linearFunction (Linear coeffs) = Text.intercalate " + " $
    (\(var, coeff) -> show coeff <> " " <> varName var) <$> Map.toList coeffs

convert :: (Ord z, Ord r, NamedVariable r, NamedVariable z) => Program z r IntDouble -> Text
convert p = Text.unlines
    [ "Minimize"
    , indent $ linearFunction (_objective p)
    , "Subject to"
    , textConstraints
    , "Bounds"
    , bounds
    , "General"
    , integerVariablesLP
    , "Binary"
    , binaryVariablesLP
    , "End"
    ]
    where
    constraints = zip (("C"<>) . show <$> [1..]) $ convertConstraints (_constraints p)
    textConstraints = Text.unlines $ indent . mkConstraint <$> constraints
    mkConstraint :: (NamedVariable z, NamedVariable r) => (Text, MPSConstraint z r IntDouble) -> Text
    mkConstraint (ix, MPSConstraint tpe function rhs) = " " <> ix <> ": " <> linearFunction function <> " " <> op <> " " <> show rhs
        where
        op = case tpe of
            Eq -> "="
            Lt -> "<="
            Gt -> ">="
    allBounds = Map.toList $ _bounds p
    bounds = Text.unlines $ catMaybes $ uncurry mkBounds <$> allBounds
    mkBounds :: (NamedVariable z, NamedVariable r) => Either z r -> (Maybe (R IntDouble), Maybe (R IntDouble)) -> Maybe Text
    mkBounds variable (Nothing, Nothing) = Nothing
    mkBounds variable (lowerBoundMb, upperBoundMb) = pure $ indent $ (maybe "" (\lb -> show lb <> " <= ") lowerBoundMb) <> varName variable <> (maybe "" (\ub -> " <= " <> show ub) upperBoundMb)

    integerVariablesLP = Text.unlines $ indent . varName <$> integerVariables
    integerVariables = mapMaybe (either Just (const Nothing)) $ Set.toList (varsOfProgram p)
    binaryVariablesLP = Text.unlines $ indent . varName <$> binaryVariables
    binaryVariables = mapMaybe filterBinary allBounds
    filterBinary (Left z, (Just 0.0, Just 1.0)) = Just z
    filterBinary _                              = Nothing

indent :: Text -> Text
indent str = "  " <> str
