module Main where

import Data.Extensible.Effect
-- import qualified Effects.Culc as Culc
-- import qualified Handler.Culc.Human as Human
import qualified Handler.Culc.Pure as Pure
import Usecase.Culc

main :: IO ()
main = do
  if leaveEff (Pure.run culc) == 10
    then putStrLn "passed"
    else error "error"

-- ans <- retractEff (Human.run (culc :: Eff '[Culc.NamedEff, "IO" >: IO] Int))
-- print ans
