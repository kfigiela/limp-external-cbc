-- http://lpsolve.sourceforge.net/5.0/CPLEX-format.htm
{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Limp.External.LP where

import           Universum          hiding (Constraint)

import           Numeric.Limp.Canon
import           Numeric.Limp.Rep

import           Data.Char          (isDigit, isLetter)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import qualified Data.Text.Lazy     as LText

class NamedVariable var where
    varName :: var -> Text
    default varName :: Show var => var -> Text
    varName = show

instance (NamedVariable z, NamedVariable r) => NamedVariable (Either z r) where
    varName = either varName varName
instance NamedVariable Text where
    varName = filterChars
instance NamedVariable LText where
    varName = filterChars . toText
instance NamedVariable String where
    varName = filterChars . toText

varNameL :: NamedVariable var => var -> LText
varNameL = toLText . varName

type LPProgram z r = (NamedVariable z, NamedVariable r, Ord z, Ord r)

filterChars :: Text -> Text
filterChars str = case Text.uncons filtered of
        Just (c, _t) | isLetter c -> filtered -- variable name has to start with letter
        _                         -> "v" <> filtered
    where
        f :: Char -> Bool
        f c = isLetter c || isDigit c || c `elem` ("_[]{}/.&#$%~'@^" :: String) -- only those chars are allowed
        filtered = Text.filter f str

linearFunction :: (NamedVariable z, NamedVariable r, Rep c, Show (R c), Show (Z c)) => Linear z r c -> LText
linearFunction (Linear coeffs) = LText.intercalate " + " $ varWithCoeff <$> Map.toList coeffs
    where
    varWithCoeff :: (NamedVariable z, NamedVariable r, Show (R c), Show (Z c), Rep c) => (Either z r, R c) -> LText
    varWithCoeff (var, coeff) | coeff == 1 = varNameL var
    varWithCoeff (var, coeff) = show coeff <> " " <> varNameL var

convertConstraint :: (NamedVariable z, NamedVariable r, Rep c, Show (R c), Show (Z c)) => Constraint1 z r c -> [LText]
convertConstraint (C1 (Just lowerBound) fun (Just upperBound)) | lowerBound == upperBound = [prettyConstraint fun "=" lowerBound]
convertConstraint (C1 lowerBoundMb fun upperBoundMb) = catMaybes [prettyConstraint fun ">=" <$> lowerBoundMb, prettyConstraint fun "<=" <$> upperBoundMb]

prettyConstraint :: (NamedVariable z, NamedVariable r, Rep c, Show (Z c), Show (R c)) => Linear z r c -> LText -> R c -> LText
prettyConstraint fun op rhs = linearFunction fun <> " " <> op <> " " <> show rhs

convert :: (Ord z, Ord r, NamedVariable r, NamedVariable z) => Program z r IntDouble -> Text
convert p = toText $ LText.unlines $ mconcat
    [ ["Minimize"]
    , indent [linearFunction (_objective p)]
    , ["Subject to"]
    , indent textConstraints
    , ["Bounds"]
    , indent bounds
    , ["General"]
    , indent integerVariables
    , ["Binary"]
    , indent binaryVariables
    , ["End"]
    ]
    where
    textConstraints = mconcat $ convertConstraint <$> constraints
    Constraint constraints = _constraints p
    allBounds = Map.toList $ _bounds p
    bounds = catMaybes $ uncurry mkBounds <$> allBounds
    mkBounds :: (NamedVariable z, NamedVariable r) => Either z r -> (Maybe (R IntDouble), Maybe (R IntDouble)) -> Maybe LText
    mkBounds variable (Nothing, Nothing) = Nothing
    mkBounds variable (lowerBoundMb, upperBoundMb) = Just $
        maybe "" (\lb -> show lb <> " <= ") lowerBoundMb
        <> varNameL variable
        <> maybe "" (\ub -> " <= " <> show ub) upperBoundMb

    integerVariables = varNameL <$> lefts (Set.toList $ varsOfProgram p)
    binaryVariables = varNameL <$> mapMaybe onlyBinary allBounds

    onlyBinary (Left z, (Just 0.0, Just 1.0)) = Just  z
    onlyBinary _                              = Nothing

indent :: [LText] -> [LText]
indent = fmap ("  " <>)
