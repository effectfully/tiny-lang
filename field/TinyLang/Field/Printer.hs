module TinyLang.Field.Printer
    ( toStringWithIDs
    , toStringNoIDs, tos
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
toStringBinOp Add = " + "
toStringBinOp Sub = " - "
toStringBinOp Mul = " * "
toStringBinOp Div = " / "

-- Do we want () round something when printing it inside some other expression?
isSimple :: Expr f a -> Bool
-- isSimple (EVal _) = True  -- No!  Rationals cause problems: for example, neq0 (-1) % 2 is hard to parse
isSimple _        = False

-- Convert to string (with enclosing () if necessary)
toString1 :: Show f => PrintStyle -> Expr f a -> String
toString1 s e = if isSimple e then toString s e else "(" ++ toString s e ++ ")"

toStringUniVal :: Show f => UniVal f a -> String
toStringUniVal (UniVal Bool  b) = if b then "T" else "F"
toStringUniVal (UniVal Field i) = show i

-- Main function
toString :: Show f => PrintStyle -> Expr f a -> String
toString _ (EVal uv)            = toStringUniVal uv
toString s (EVar _ v)           = toStringVar s v
toString s (EAppUnOp op e)      = toStringUnOp op ++ toString1 s e
toString s (EAppBinOp op e1 e2) = toString1 s e1 ++ toStringBinOp op ++ toString1 s e2
toString s (EIf e e1 e2)        = "if " ++ toString1 s e ++ " then " ++ toString1 s e1 ++ " else " ++ toString1 s e2

-- | Convert an Expr to a String, ignoring Unique IDs in variable names
toStringNoIDs :: Show f => Expr f a -> String
toStringNoIDs = toString NoIDs

-- | Convert an Expr to a String, including Unique IDs in variable names
toStringWithIDs :: Show f => Expr f a -> String
toStringWithIDs = toString WithIDs

tos :: (Show f) => SomeUniExpr f -> String
tos (SomeUniExpr _ e) = toStringNoIDs e
