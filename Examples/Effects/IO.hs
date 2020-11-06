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
import Data.Extensible.Effect.TH.Util (mkEffTypes)

$(mkEffTypes ''IO)

{-
  Generated definitions.

    type EffName = "IO"
    type AnonEff = IO
    type NamedEff = (>:) EffName AnonEff
    type HasEff effs = Lookup effs EffName AnonEff
    lift :: HasEff effs => AnonEff a -> Eff effs a
    lift = liftEff (Proxy @EffName)
-}