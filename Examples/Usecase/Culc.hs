module Usecase.Culc where

import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import qualified Effects.Culc as Culc

culc ::
  Culc.HasEff effs =>
  Eff effs Int
culc = do
  a <- 10 `Culc.add` 10
  b <- a `Culc.sub` 5
  c <- b `Culc.mul` 2
  d <- c `Culc.div` 3
  return d
