module Effects.IO
  ( AnonEff,
    EffName,
    HasEff,
    NamedEff,
    lift,
  )
where

import Data.Extensible
import Data.Extensible.Effect
import TH (mkEffTypes)

$(mkEffTypes ''IO)

{-
    type EffName = "IO"
    type AnonEff = IO
    type NamedEff = (>:) EffName AnonEff
    type HasEff effs = Lookup effs EffName AnonEff
    lift :: HasEff effs => AnonEff x -> Eff effs x
    lift = liftEff (Proxy @EffName)
-}