module Effects.Culc
  ( Culc (..),
    AnonEff,
    EffName,
    HasEff,
    NamedEff,
    lift,
    add,
    sub,
    mul,
    div,
  )
where

import Data.Extensible
import Data.Extensible.Effect
import TH (decEffects)
import Prelude hiding (div)

decEffects
  [d|
    data Culc x where
      Add :: Int -> Int -> Culc Int
      Sub :: Int -> Int -> Culc Int
      Mul :: Int -> Int -> Culc Int
      Div :: Int -> Int -> Culc Int
    |]

{-
  Generated definitions.

    data Culc x_a69m
      where
        Add :: Int -> Int -> Culc Int
        Sub :: Int -> Int -> Culc Int
        Mul :: Int -> Int -> Culc Int
        Div :: Int -> Int -> Culc Int

    type EffName = "Culc"
    type AnonEff = Culc
    type NamedEff = (>:) EffName AnonEff
    type HasEff effs = Lookup effs EffName AnonEff

    lift :: HasEff effs => AnonEff x -> Eff effs x
    lift = liftEff (Proxy @EffName)

    add :: HasEff effs => Int -> Int -> Eff effs Int
    add a b = lift ((Add a) b)

    sub :: HasEff effs => Int -> Int -> Eff effs Int
    sub a b = lift ((Sub a) b)

    mul :: HasEff effs => Int -> Int -> Eff effs Int
    mul a b = lift ((Mul a) b)

    div :: HasEff effs => Int -> Int -> Eff effs Int
    div a b = lift ((Div a) b)
-}