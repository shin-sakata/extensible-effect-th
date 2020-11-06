module Handler.Culc.Human where

import Data.Extensible.Effect
import Effects.Culc (Culc (..))
import qualified Effects.Culc as Culc
import qualified Effects.IO as IO

run ::
  forall effs a.
  IO.HasEff effs =>
  Eff (Culc.NamedEff ': effs) a ->
  Eff effs a
run effs = peelEff0 pure interpret effs
  where
    interpret :: forall r. Culc.AnonEff r -> (r -> Eff effs a) -> Eff effs a
    interpret (Add n m) k = do
      IO.lift $ putStrLn (show n <> " + " <> show m)
      ans <- IO.lift getLine
      k (read ans)
    interpret (Sub n m) k = do
      IO.lift $ putStrLn (show n <> " - " <> show m)
      ans <- IO.lift getLine
      k (read ans)
    interpret (Mul n m) k = do
      IO.lift $ putStrLn (show n <> " * " <> show m)
      ans <- IO.lift getLine
      k (read ans)
    interpret (Div n m) k = do
      IO.lift $ putStrLn (show n <> " / " <> show m)
      ans <- IO.lift getLine
      k (read ans)
