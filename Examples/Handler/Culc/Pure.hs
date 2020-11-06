module Handler.Culc.Pure where

import Data.Extensible.Effect
import Effects.Culc (Culc (..))
import qualified Effects.Culc as Culc

run ::
  forall effs a.
  Eff (Culc.NamedEff ': effs) a ->
  Eff effs a
run effs = peelEff0 pure interpret effs
  where
    interpret :: forall r. Culc.AnonEff r -> (r -> Eff effs a) -> Eff effs a
    interpret (Add n m) k = do
      k (n + m)
    interpret (Sub n m) k = do
      k (n - m)
    interpret (Mul n m) k = do
      k (n * m)
    interpret (Div n m) k = do
      k (n `div` m)
