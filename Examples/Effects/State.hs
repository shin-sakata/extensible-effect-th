module Effects.State
  ( State (..),
    AnonEff,
    EffName,
    HasEff,
    NamedEff,
    lift,
    get,
    put,
  )
where

import Data.Extensible
import Data.Extensible.Effect hiding (State, get)
import TH (decEffects)
import Prelude hiding (div)

decEffects
  [d|
    data State s a where
      Get :: State s s
      Put :: s -> State s ()
    |]

{-
    data State s_a6Q9 a_a6Qa
      where
        Get :: State s_a6Qb s_a6Qb
        Put :: s_a6Qc -> State s_a6Qc ()

    type EffName = "State"
    type AnonEff = State
    type NamedEff s = (>:) EffName (AnonEff s)
    type HasEff s effs = Lookup effs EffName (AnonEff s)

    lift :: HasEff s effs => AnonEff s a -> Eff effs a
    lift = liftEff (Proxy @EffName)

    get :: HasEff s effs => Eff effs s
    get = lift Get

    put :: HasEff s effs => s -> Eff effs ()
    put a = lift (Put a)
-}