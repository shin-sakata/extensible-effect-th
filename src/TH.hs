module TH where

import Data.Extensible.Effect
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

dataDecs2func :: [Dec] -> [Dec]
dataDecs2func [(DataD _ _ _ _ _ _)] = undefined

-- |
-- >>> tyVarBndrs2ctx [PlainTV (mkName "s"), PlainTV (mkName "a")]
-- [AppT (AppT (ConT HasEff) (VarT a)) (VarT s)]
--
-- >>> tyVarBndrs2ctx [PlainTV (mkName "s"), PlainTV (mkName "env"), PlainTV (mkName "a")]
-- [AppT (AppT (AppT (ConT HasEff) (VarT a)) (VarT env)) (VarT s)]
tyVarBndrs2ctx :: [TyVarBndr] -> Cxt
tyVarBndrs2ctx tyVarBndrs = pure $ foldr f (ConT (mkName "HasEff")) tyVarBndrs
  where
    f :: TyVarBndr -> Pred -> Pred
    f (PlainTV l) r = r `AppT` (VarT l)

-- forall <vars>. <ctxt> => <type>
-- gadtC2typeWithName :: Name -> Con ->