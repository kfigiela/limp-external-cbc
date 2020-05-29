{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Universum

import           Control.Monad
import qualified Data.Map                                as M

import           Numeric.Limp.Canon                      as C
import           Numeric.Limp.Canon.Convert              as Convert (program)
import           Numeric.Limp.External.LP
import           Numeric.Limp.Program                    as P
import           Numeric.Limp.Rep                        as R
import           Numeric.Limp.Solve.Simplex.Maps         as SM
import           Numeric.Limp.Solve.Simplex.StandardForm as ST
import           Numeric.Limp.Solvers.ExternalCbc        (solve)


-- | Integral variable
zr :: Rep c => z -> R c -> P.Linear z r c 'KR
zr z' c
 = LR [(Left z', c)] 0

-- data Xs = X1 | X3
--   deriving (Eq, Ord, Show, NamedVariable)

-- data Zs = X2
--   deriving (Eq, Ord, Show, NamedVariable)

-- instance NamedVariable () where varName () = "unit"

-- prog :: P.Program Zs Xs R.IntDouble
-- prog
--  = P.maximise
--     -- objective
--         (r X1 60 .+. z X2  30 .+. r X3  20)
--     -- subject to
--      (   r X1  8 .+. z X2   6 .+. r X3   1 :<= con 48
--      :&& r X1  2 .+. zr X2 1.5 .+. r X3 0.5 :<= con  8
--      :&& r X1  4 .+. z X2   2 .+. r X3 1.5 :<= con 20
--      :&&             z X2   1              :<= con  5)
--     -- bounds ommitted for now
--     [ lowerR 0 X1 , binary X2 , lowerR 0 X3 ]
--     -- []


xkcd :: P.Program String String R.IntDouble
xkcd = P.minimise
           ( z1 mf .+.
             z1 ff .+.
             z1 ss .+.
             z1 hw .+.
             z1 ms .+.
             z1 sp )
           ( z mf mfp .+.
             z ff ffp .+.
             z ss ssp .+.
             z hw hwp .+.
             z ms msp .+.
             z sp spp :== con 1505 )
           [ lowerZ 0 mf
           , lowerZ 0 ff
           , lowerZ 0 ss
           , lowerZ 0 hw
           , lowerZ 0 ms
           , lowerZ 0 sp
            ]
  where
    (mf, mfp) = ("2mixed-Fruit",       215)
    (ff, ffp) = ("frenchFries",      275)
    (ss, ssp) = ("sideSalad",        335)
    (hw, hwp) = ("hotWings",         355)
    (ms, msp) = ("mozzarellaSticks", 420)
    (sp, spp) = ("samplerPlate",     580)

problem = Convert.program $ xkcd

main :: IO ()
main = do
    putTextLn $ convert $ problem
    solu <- solve problem
    putTextLn $ show $ solu
    pass
