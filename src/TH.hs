module TH (decEffects, customDecEffectsUtils, mkEffTypes, mkEffTypes1, mkEffTypes2) where

import Control.Monad (replicateM)
import Data.Char (toLower, toUpper)
import Data.Extensible.Effect
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

decEffects :: DecsQ -> DecsQ
decEffects = customDecEffectsUtils True True

customDecEffectsUtils :: Bool -> Bool -> DecsQ -> DecsQ
customDecEffectsUtils synTypes synFuncs decsq = do
  gadtss <- decsq
  case gadtss of
    [gadts] -> do
      utilDecs <- gadt2utilTypes gadts
      let liftDecs = gadt2lift gadts
      funcDecs <- gadts2funcsD gadts
      pure $
        [constGadts gadts]
          <> [dec | dec <- utilDecs, synTypes]
          <> liftDecs
          <> [dec | dec <- funcDecs, synFuncs]
    _ -> error "Multiple GADTs definitions are not yet supported."

mkEffTypes :: Name -> DecsQ
mkEffTypes = mkEffTypes1

mkEffTypes1 :: Name -> DecsQ
mkEffTypes1 name =
  pure
    [ name2EffName (getName $ mkName $ nameBase name),
      name2AnonEff (getName $ mkName $ nameBase name),
      tyVarBndrs2NamedEff [PlainTV (mkName "a")],
      tyVarBndrs2HasEff [PlainTV (mkName "a")],
      tyVarBndrs2liftSig [PlainTV (mkName "a")],
      liftVal
    ]

mkEffTypes2 :: Name -> DecsQ
mkEffTypes2 name =
  pure
    [ name2EffName (getName $ mkName $ nameBase name),
      name2AnonEff (getName $ mkName $ nameBase name),
      tyVarBndrs2NamedEff [PlainTV (mkName "a"), PlainTV (mkName "b")],
      tyVarBndrs2HasEff [PlainTV (mkName "a"), PlainTV (mkName "b")],
      tyVarBndrs2liftSig [PlainTV (mkName "a"), PlainTV (mkName "b")],
      liftVal
    ]

gadts2funcsD :: Dec -> DecsQ
gadts2funcsD (DataD cxt name tyVarBndr _ constructors _) = do
  decs <- mapM f constructors
  pure $ concat decs
  where
    f :: Con -> DecsQ
    f con@(GadtC [conName] _ _) = do
      clause <- (gadtC2Clause con)
      pure
        [ SigD (toLowerHead $ getName conName) (ForallT [] (tyVarBndrs2ctxt tyVarBndr) (gadtC2type con)),
          FunD (toLowerHead $ getName conName) [clause]
        ]

-- |
-- forall <vars>. <ctxt> => <type>
--                 ^^^^
-- ctxt is above.
-- >>> tyVarBndrs2ctxt [PlainTV (mkName "a"), PlainTV (mkName "b"), PlainTV (mkName "x")]
-- [AppT (AppT (AppT (ConT HasEff) (VarT a)) (VarT b)) (VarT effs)]
tyVarBndrs2ctxt :: [TyVarBndr] -> Cxt
tyVarBndrs2ctxt tyVarBndrs =
  [(foldl f (ConT (mkName "HasEff")) (map unPlainTV (init tyVarBndrs))) `AppT` (VarT (mkName "effs"))]
  where
    f :: Pred -> Name -> Pred
    f l r = l `AppT` (VarT r)

-- |
-- forall <vars>. <ctxt> => <type>
--                           ^^^^
-- type is above.
-- >>> let constructorName = [mkName "Blah"]
-- >>> let bangTypes = [(Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "Int")), (Bang NoSourceUnpackedness NoSourceStrictness, VarT (mkName "a"))]
-- >>> let typ = (AppT (AppT (AppT (ConT (mkName "Blah")) (VarT (mkName "a"))) (VarT (mkName "b"))) (VarT (mkName "b")))
-- >>> let gadt = GadtC constructorName bangTypes typ
-- >>> gadtC2type gadt
-- AppT (AppT ArrowT (ConT Int)) (AppT (AppT ArrowT (VarT a)) (AppT (AppT (ConT Eff) (VarT effs)) (VarT b)))
gadtC2type :: Con -> Type
gadtC2type (GadtC _ bangtypes typ) =
  foldr f (g typ) bangtypes
  where
    f :: BangType -> Pred -> Pred
    f (_, l@(ConT _)) r = AppT (AppT ArrowT l) r
    f (_, l@(VarT name)) r = AppT (AppT ArrowT (VarT (getName name))) r
    g :: Pred -> Pred
    g (AppT _ last@(VarT name)) = (AppT (AppT (ConT (mkName "Eff")) (VarT (mkName "effs"))) $ VarT $ getName name)
    g (AppT _ last@(ConT name)) = (AppT (AppT (ConT (mkName "Eff")) (VarT (mkName "effs"))) $ ConT $ getName $ mkName $ nameBase name)
    g (AppT _ last) = (AppT (AppT (ConT (mkName "Eff")) (VarT (mkName "effs"))) last)

---------------------------------------------------------------------------------------------------------

gadtC2Clause :: Con -> Q Clause
gadtC2Clause con@(GadtC _ bangtypes typ) = do
  let names = map snd (zip bangtypes uniqueNames)
  let pats = namesC2pats names
  let body = gadtCWithNames2body con names
  pure (Clause pats body [])

namesC2pats :: [Name] -> [Pat]
namesC2pats names =
  map VarP names

-- |
-- >>> let constructorName = [mkName "Blah"]
-- >>> let bangTypes = [(Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "Int")), (Bang NoSourceUnpackedness NoSourceStrictness, VarT (mkName "a"))]
-- >>> let typ = (AppT (AppT (AppT (ConT (mkName "Blah")) (VarT (mkName "a"))) (VarT (mkName "b"))) (VarT (mkName "b")))
-- >>> con = GadtC constructorName bangTypes typ
-- >>> gadtCWithNames2body con [(mkName "int"), (mkName "a")]
-- NormalB (AppE (VarE lift) (AppE (AppE (UnboundVarE Blah) (VarE int)) (VarE a)))
gadtCWithNames2body :: Con -> [Name] -> Body
gadtCWithNames2body (GadtC [name] _ typ) args =
  NormalB (AppE (VarE (mkName "lift")) (foldl f (UnboundVarE $ getName name) args))
  where
    f :: Exp -> Name -> Exp
    f l name = AppE l (VarE name)

-- ( AppE
--     (VarE (mkName "lift"))
--     (AppE (AppE (UnboundVarE name) (VarE (mkName "int"))) (VarE (mkName "a")))
-- )

---------------------------------------------------------------------------------------------------------

toLowerHead :: Name -> Name
toLowerHead name = mkName (toLowerHeadStr (show name))

toLowerHeadStr :: String -> String
toLowerHeadStr [] = []
toLowerHeadStr (h : t) = (toLower h) : t

getName :: Name -> Name
getName name = do
  mkName (head (splitBy (== '_') (show name)))

useName' :: TyVarBndr -> TyVarBndr
useName' (PlainTV name) = PlainTV $ getName name

unPlainTV :: TyVarBndr -> Name
unPlainTV (PlainTV name) = getName name

uniqueNames :: [Name]
uniqueNames = map mkName $ concatMap (flip replicateM ['a' .. 'z']) [1 ..]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p [] = []
splitBy p xs = a : (splitBy p $ dropWhile p $ b)
  where
    (a, b) = break p xs

---------------------------------------------------------------------------------------------------------

gadt2utilTypes :: Dec -> DecsQ
gadt2utilTypes (DataD cxt name tyVarBndr _ constructors _) =
  pure
    [ name2EffName name,
      name2AnonEff name,
      tyVarBndrs2NamedEff tyVarBndr,
      tyVarBndrs2HasEff tyVarBndr
    ]

name2EffName :: Name -> Dec
name2EffName name = TySynD (mkName "EffName") [] (LitT (StrTyLit (show (getName name))))

name2AnonEff :: Name -> Dec
name2AnonEff name = TySynD (mkName "AnonEff") [] (ConT (getName name))

tyVarBndrs2NamedEff :: [TyVarBndr] -> Dec
tyVarBndrs2NamedEff tyVarBndrs = do
  let tyVarBndrs' = map useName' tyVarBndrs
  TySynD (mkName "NamedEff") (init tyVarBndrs') (tyVarBndr2typ tyVarBndrs')
  where
    tyVarBndr2typ :: [TyVarBndr] -> Type
    tyVarBndr2typ tyVarBndrs' =
      AppT
        (AppT (ConT (mkName ">:")) (ConT (mkName "EffName")))
        (foldl f (ConT (mkName "AnonEff")) (init tyVarBndrs'))

    f :: Type -> TyVarBndr -> Type
    f l (PlainTV r) = AppT l (VarT r)

tyVarBndrs2HasEff :: [TyVarBndr] -> Dec
tyVarBndrs2HasEff tyVarBndrs = do
  TySynD
    (mkName "HasEff")
    hasEfftyVarBndrs
    typ
  where
    hasEfftyVarBndrs :: [TyVarBndr]
    hasEfftyVarBndrs = init (map useName' tyVarBndrs) <> [PlainTV (mkName "effs")]

    typ :: Type
    typ = (AppT (AppT (AppT (ConT (mkName "Lookup")) (VarT (mkName "effs"))) (ConT (mkName "EffName"))) (tyVarBndr2typ $ init (map useName' tyVarBndrs)))

    tyVarBndr2typ :: [TyVarBndr] -> Type
    tyVarBndr2typ tyVarBndrs' =
      (foldl f (ConT (mkName "AnonEff")) tyVarBndrs')

    f :: Type -> TyVarBndr -> Type
    f l (PlainTV r) = AppT l (VarT r)

---------------------------------------------------------------------------------------------------------
gadt2lift :: Dec -> [Dec]
gadt2lift gadt@(DataD cxt name tyVarBndrs _ constructors _) =
  [ tyVarBndrs2liftSig tyVarBndrs,
    liftVal
  ]

tyVarBndrs2liftSig :: [TyVarBndr] -> Dec
tyVarBndrs2liftSig tyVarBndrs =
  SigD
    (mkName "lift")
    ( ForallT
        []
        (tyVarBndrs2ctxt tyVarBndrs)
        (typ tyVarBndrs)
    )
  where
    typ :: [TyVarBndr] -> Type
    typ tyVarBndr =
      ( AppT
          (AppT ArrowT (foldl f (ConT (mkName "AnonEff")) tyVarBndr))
          (AppT (AppT (ConT (mkName "Eff")) (VarT (mkName "effs"))) (VarT (unPlainTV (last tyVarBndr))))
      )
    f :: Type -> TyVarBndr -> Type
    f l (PlainTV r) = AppT l (VarT $ getName r)

liftVal :: Dec
liftVal =
  ValD
    (VarP (mkName "lift"))
    (NormalB (AppE (VarE (mkName "liftEff")) (AppTypeE (ConE (mkName "Proxy")) (ConT (mkName "EffName")))))
    []

---------------------------------------------------------------------------------------------------------

constGadts :: Dec -> Dec
constGadts (DataD cxt name tyVarBndr kind constructors dc) =
  (DataD cxt (getName name) tyVarBndr kind (fmap constConstructor constructors) dc)
  where
    constConstructor :: Con -> Con
    constConstructor (GadtC [name] bt typ) = (GadtC [getName name] bt (constTyp typ))

    constTyp :: Type -> Type
    constTyp (ConT name) = (ConT $ getName $ mkName $ nameBase name)
    constTyp t@(TupleT n) = t
    constTyp t@(VarT _) = t
    constTyp (AppT r l) = AppT (constTyp r) (constTyp l)
    constTyp err = error $ show err
