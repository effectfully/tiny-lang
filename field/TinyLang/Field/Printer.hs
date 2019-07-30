module TinyLang.Field.Printer
    ( PrintStyle(..)
    , exprToString
    , someExprToString
    ) where

import           TinyLang.Field.Core
import           TinyLang.Var

-- | Variable names are equipped with Unique identifiers.  The
-- PrintStyle type determines whether printed variable names include
-- these or not ("x_5" versus "x").  If we're going to re-parse the
-- output of toString we probably don't want the IDs.
data PrintStyle = WithIDs | NoIDs

toStringVar :: PrintStyle -> Var -> String
toStringVar NoIDs   (Var _ name) = name
toStringVar WithIDs v            = show v   -- or explicitly tell it what to do?

toStringUnOp :: UnOp f a b -> String
toStringUnOp Not  = "not "
toStringUnOp Neq0 = "neq0 "
toStringUnOp Neg  = "neg "
toStringUnOp Inv  = "inv "

toStringBinOp :: BinOp f a b c -> String
toStringBinOp Or  = " or "
toStringBinOp And = " and "
toStringBinOp Xor = " xor "
toStringBinOp FEq = " == "
toStringBinOp FLt = " < "
toStringBinOp FLe = " <= "
toStringBinOp FGe = " >= "
toStringBinOp FGt = " > "
toStringBinOp Add = " + "
toStringBinOp Sub = " - "
toStringBinOp Mul = " * "
toStringBinOp Div = " / "

-- Do we want () round something when printing it inside some other expression?
isSimple :: Expr f a -> Bool
-- isSimple (EVal _) = True  -- No!  Rationals cause problems: for example, neq0 (-1) % 2 is hard to parse
isSimple _        = False

-- Convert to string (with enclosing () if necessary)
exprToString1 :: Show f => PrintStyle -> Expr f a -> String
exprToString1 s e = if isSimple e then exprToString s e else "(" ++ exprToString s e ++ ")"

toStringUniVal :: Show f => UniVal f a -> String
toStringUniVal (UniVal Bool  b) = if b then "T" else "F"
toStringUniVal (UniVal Field i) = show i

-- Main function
exprToString :: Show f => PrintStyle -> Expr f a -> String
exprToString _ (EVal uv)            = toStringUniVal uv
exprToString s (EVar _ v)           = toStringVar s v
exprToString s (EAppUnOp op e)      = toStringUnOp op ++ exprToString1 s e
exprToString s (EAppBinOp op e1 e2) = exprToString1 s e1 ++ toStringBinOp op ++ exprToString1 s e2
exprToString s (EIf e e1 e2)        = concat
    [ "if "
    , exprToString1 s e
    , " then "
    , exprToString1 s e1
    , " else "
    , exprToString1 s e2
    ]
exprToString s (ELet uni var def e) = concat
    [ "let "
    , toStringVar s var
    , " :: "
    , show uni
    , " = "
    , exprToString s def
    , "; "
    , exprToString s e
    ]

someExprToString :: (Show f) => PrintStyle -> SomeUniExpr f -> String
someExprToString s (SomeUniExpr _ e) = exprToString s e
