module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

main :: IO ()
main = do
  dec' <- dec
  print dec'

type HasEff = IO

type State = IO

type Eff = IO

dec =
  runQ
    [d|
      put :: HasEff s effs => State s a -> Eff effs a
      put s = lift (Put s)
      |]

-- data State s a where
--   Put :: s -> State s ()
--   Get :: State s s
-- dec'' =
--   [ DataD
--       []
--       State_0
--       [PlainTV s_3, PlainTV a_4]
--       Nothing
--       [ GadtC [Put_1] [(Bang NoSourceUnpackedness NoSourceStrictness, VarT s_5)] (AppT (AppT (ConT State_0) (VarT s_5)) (TupleT 0)),
--         GadtC [Get_2] [] (AppT (AppT (ConT State_0) (VarT s_6)) (VarT s_6))
--       ]
--       []
--   ]

-- put :: HasEff s effs => State s a -> Eff effs a
-- put s = lift (Put s)
-- funcDec name =
--   [ SigD
--       name
--       ( ForallT
--           []
--           [AppT (AppT (ConT Main.HasEff) (VarT s_0)) (VarT effs_1)]
--           (AppT (AppT ArrowT (AppT (AppT (ConT Main.State) (VarT s_0)) (VarT a_2))) (AppT (AppT (ConT Main.Eff) (VarT effs_1)) (VarT a_2)))
--       ),
--     FunD name [Clause [VarP s_4] (NormalB (AppE (VarE Language.Haskell.TH.Syntax.lift) (AppE (UnboundVarE Put) (VarE s_4)))) []]
--   ]
